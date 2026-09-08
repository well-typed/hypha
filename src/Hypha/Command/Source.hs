{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Hypha.Command.Source
  ( -- * Types
    SourceResult (..)
  , DefinedIn (..)
    -- * Field sets
  , compactKeys
  , fullKeys
    -- * Execution
  , runSource
  , runSourceFromDir
    -- * Shared symbol location
  , SymbolSite (..)
  , siteLocation
  , locateSymbolSite
  , symbolNotFound
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.Aeson qualified as Aeson
import Data.Aeson (Value (..), (.=))
import Data.Set qualified as Set
import Data.Set (Set)
import Data.Text qualified as Text
import Data.Text (Text)
import Hypha.Encoding (readSourceFile)
import Hypha.BuildEnv.Type (BuildEnv (..))
import Hypha.Cli.Types
import Hypha.Error (HyphaError (..), NotFoundReason (..))
import Hypha.Output.Outcome (Outcome, successOutcome)
import Hypha.Project.BuildContext (BuildContext)
import Hypha.Project.Components qualified as Comp
import Hypha.Search.Index (ModuleSource (..))
import Hypha.Search.Indexer qualified as Indexer
import Hypha.Types.ComponentName
  (ComponentKey (..), componentKeyOf)
import Hypha.Source.Locate
  ( LocatedDefinition (..), SourceLocation (..), findModuleFile
  , locateDefinitionInComponent, locateSymbolDefinitionInDir )
import Hypha.Source.Extensions qualified as Extensions
import Hypha.Source.Parser qualified as Parser
import Hypha.Source.Reach
  ( OutsideReach (..), SymbolSearchFailure (..) )
import Hypha.Types.SymbolPath (ModulePath (..), SymbolName (..))
import System.IO (hPutStrLn, stderr)
import Hypha.Types.PackageId (PackageId (..), PackageName (..), Version (..))

-- | What the returned snippet is cut from.
--
-- A caller has to be able to tell an exact answer from a fallback: the
-- command used to return a flat 30-line window in every case, which
-- reads like a declaration's source but trails off into whatever
-- followed it (issue #55).
data SpanKind
  = SpanDeclaration
    -- ^ The declaration's own lines, doc comment included.
  | SpanModuleHeader
    -- ^ The module's @-- |@ block, @module M@ clause and export list,
    -- for a request that named no symbol.
  | SpanWindow
    -- ^ Neither was available: a package scan found a line number and
    -- nothing more, or the module would not parse.  Lines either side of
    -- the target, and the caller is told as much rather than being left
    -- to infer it.
  deriving stock (Show, Eq)

-- | How 'SpanKind' reaches the wire.  Rendering lives here, at the
-- boundary, not at the call sites that build the value.
spanKindText :: SpanKind -> Text
spanKindText = \case
  SpanDeclaration  -> "declaration"
  SpanModuleHeader -> "module_header"
  SpanWindow       -> "window"

-- | The lines a snippet covers, and what determined them.
data SnippetSpan = SnippetSpan
  { ssKind  :: !SpanKind
  , ssStart :: !Int
  , ssEnd   :: !Int
  }
  deriving stock (Show, Eq)

-- | Result of a source command.
data SourceResult = SourceResult
  { srcPackage   :: !Text
    -- ^ Package name.
  , srcVersion   :: !Text
    -- ^ Package version.
  , srcModule    :: !Text
    -- ^ Module path.
  , srcSymbol    :: !(Maybe Text)
    -- ^ Symbol name (if provided).
  , srcPath      :: !FilePath
    -- ^ Path to the source file.
  , srcLine      :: !Int
    -- ^ Line number of the definition.
  , srcSnippet   :: !Text
    -- ^ The source the span covers.  Not clamped: @hypha source@ exists
    -- to hand a caller the whole of something, and a declaration
    -- truncated at an arbitrary line is the problem this command had.
  , srcSpan      :: !SnippetSpan
    -- ^ Which lines 'srcSnippet' is, and whether they are the
    -- declaration's own.
  , srcDefinedIn :: !(Maybe DefinedIn)
    -- ^ Where the definition actually turned out to be, when that is not
    -- the module the caller named.
    --
    -- @hypha source base\/Data.List\/sortOn@ answers with a path inside
    -- @ghc-internal@, and reporting only @module: Data.List@ beside it
    -- told the reader something false about a path they could see: a
    -- follow-up @hypha source base\/Data.List\/…@ on the module we named
    -- goes nowhere.  Absent when the definition is in the module asked
    -- for, which is the common case and needs no annotation.
  }
  deriving stock (Show)

-- | The module and component a definition turned out to live in.
data DefinedIn = DefinedIn
  { diModule    :: !ModulePath
  , diComponent :: !ComponentKey
  }
  deriving stock (Show, Eq)

-- | Compact field set.
compactKeys :: Set Text
compactKeys = Set.fromList
  [ "package", "module", "symbol", "path", "line", "snippet", "span"
  -- In the compact set because the compact set is what an agent reads by
  -- default, and a path in another package beside an unannotated module
  -- name is precisely where it would be misled.
  , "defined_in" ]

-- | Full field set.
fullKeys :: Set Text
fullKeys = Set.fromList
  [ "package", "version", "module", "symbol", "path", "line", "snippet"
  , "span"
  , "defined_in" ]

-- | Execute the source command.
--
--   Parses the argument as @PKG/MOD[/SYM]@ and returns a 30-line snippet
--   around the symbol definition (or the module header if no symbol).
runSource
  :: BuildContext -> BuildEnv IO -> OutsideReach IO -> PackageId -> Text
  -> Maybe Text
  -> IO (Either HyphaError (Outcome Value))
runSource ctx env reach pid modPath mSym = runExceptT $ do
  srcDir <- liftMaybe (NotFound (NotFoundSource pid))
    =<< liftIO (locatePackageSource env pid)
  sourceFromDirE ctx reach pid srcDir modPath mSym

-- | Variant that takes an already-resolved source directory.  Used by the
-- 'PackageResolver'-driven dispatch path so the full fallback chain (plan
-- → store → Hackage tarball) can locate sources before this command runs.
runSourceFromDir
  :: BuildContext
    -- ^ How the plan says this package's sources are read.
  -> OutsideReach IO
    -- ^ The dependency closure, for a re-export that leaves the package.
  -> PackageId
  -> FilePath   -- ^ Source directory (resolved upstream).
  -> Text       -- ^ Module path (dotted).
  -> Maybe Text -- ^ Optional symbol name.
  -> IO (Either HyphaError (Outcome Value))
runSourceFromDir ctx reach pid srcDir modPath mSym =
  runExceptT (sourceFromDirE ctx reach pid srcDir modPath mSym)

-- | Shared ExceptT body: find the module file, locate the (optional)
-- symbol, then build the snippet.
sourceFromDirE
  :: BuildContext -> OutsideReach IO -> PackageId -> FilePath -> Text
  -> Maybe Text
  -> ExceptT HyphaError IO (Outcome Value)
sourceFromDirE ctx reach pid srcDir modPath mSym = do
  -- Split, because the two cases fail differently and the merged version
  -- could build a 'NotFoundSymbol' carrying an empty symbol name -- a
  -- value no caller could ever produce.  It also stopped locating a
  -- module the component list would have found: 'findModuleFile' skips
  -- @test\/@ and @bench\/@, so a symbol in a test-suite module failed
  -- before the cabal-driven lookup ran at all.
  (path, line, snippet, spanned, definedIn) <- case mSym of
    Nothing  -> moduleHeaderSnippet ctx srcDir modPath
    Just sym -> do
      located <- liftIO
        (locateSymbolSite ctx reach pid srcDir
           (ModulePath modPath) (SymbolName sym))
      site <- case located of
        Right s  -> pure s
        Left err -> throwE
          =<< liftIO (symbolNotFound reach pid (ModulePath modPath)
                        (SymbolName sym) err)
      (snippet, spanned) <- liftIO (declarationSnippet site)
      let loc = siteLocation site
      pure ( slPath loc, slLine loc, snippet, spanned
           , definedElsewhere (ModulePath modPath) site )
  let result = SourceResult
        { srcPackage   = unPackageName (pkgName pid)
        , srcVersion   = unVersion (pkgVersion pid)
        , srcModule    = modPath
        , srcSymbol    = mSym
        , srcPath      = path
        , srcLine      = line
        , srcSnippet   = snippet
        , srcSpan      = spanned
        , srcDefinedIn = definedIn
        }
  pure (successOutcome SourceCmd (sourceResultToJSON result))

-- | The declaration's own lines, when the site resolved to one.
--
-- A resolved site carries both the declaration and the module's text
-- already — the parse that located the symbol produced them — so this
-- neither re-reads the file nor re-parses it.  That duplicate read is
-- what 'SymbolSite' was split to prevent, and this call site was still
-- doing it.
declarationSnippet :: SymbolSite -> IO (Text, SnippetSpan)
declarationSnippet = \case
  ResolvedSite ld
    | Just spn <- Parser.declSourceSpan (ldDecl ld) ->
        pure (sliceSpan SpanDeclaration spn (ldContent ld))
    -- Located, but the parse anchored none of its lines.  A window is
    -- all we have, and it is reported as one.
    | otherwise ->
        pure (windowAround (slLine (ldLocation ld)) (ldContent ld))
  -- A package scan yields a line number and nothing more: no
  -- declaration, so no span to cut.
  SweptSite loc -> do
    content <- readSourceFile (slPath loc)
    pure (windowAround (slLine loc) content)

-- | The module header, for a request that named no symbol: its doc
-- block, @module M@ clause and export list.
--
-- Parsed through the component that lists the module, so the settings
-- and the text are the ones the indexer would use.  A module no
-- component lists, or one that will not parse, falls back to a window at
-- the top of the file and says so.
moduleHeaderSnippet
  :: BuildContext -> FilePath -> Text
  -> ExceptT HyphaError IO
       (FilePath, Int, Text, SnippetSpan, Maybe DefinedIn)
moduleHeaderSnippet ctx srcDir modPath = do
  comps <- liftIO (Indexer.packageSources srcDir ctx)
  let listed =
        [ (Comp.ciLanguageSettings ci, msPath ms, msContent ms)
        | (ci, sources) <- comps
        , ms <- sources
        , msDeclaredName ms == ModulePath modPath
        ]
  (langs, path, content) <- case listed of
    (found : _) -> pure found
    -- No cabal component lists it.  A store path with no cabal file is a
    -- real case, so the module is still readable — just without the
    -- component's settings, exactly as the CLI's other ad-hoc source
    -- reads work.
    [] -> do
      filePath <- liftMaybe
        (NotFound (NotFoundModuleFileUnder srcDir modPath))
        =<< liftIO (findModuleFile srcDir modPath)
      content <- liftIO (readSourceFile filePath)
      pure (Extensions.defaultLanguageSettings, filePath, content)

  spanned <- case Parser.parseModuleWith langs path content of
    Right (hsMod, _, _) -> pure (Parser.moduleHeaderSpan hsMod)
    Left err -> do
      -- The window below is a degraded answer, so the reason for it is
      -- reported rather than swallowed.
      liftIO $ hPutStrLn stderr $
        "hypha: " <> path <> " could not be parsed: "
          <> Text.unpack (Parser.parseErrorMessage err)
          <> "; answering with the top of the file instead of its header"
      pure Nothing

  pure $ case spanned of
    Just spn ->
      let (snippet, taken) = sliceSpan SpanModuleHeader spn content
      in (path, ssStart taken, snippet, taken, Nothing)
    Nothing ->
      let (snippet, taken) = windowAround 1 content
      in (path, ssStart taken, snippet, taken, Nothing)

-- | Where the definition landed, when that is somewhere other than the
-- module the caller named.
definedElsewhere :: ModulePath -> SymbolSite -> Maybe DefinedIn
definedElsewhere asking site = case site of
  SweptSite _ -> Nothing
  ResolvedSite ld
    | ldModule ld == asking -> Nothing
    | otherwise -> Just (DefinedIn (ldModule ld) (ldComponent ld))

-- | Where a symbol turned out to be, and how we know.
--
-- Two ways, kept apart because they answer different amounts.  Resolution
-- through a component's exports yields the declaration itself, so a caller
-- building a symbol card reads the signature and Haddock off the parse
-- that located it; a package scan yields a line number and nothing more.
-- Collapsing them to a 'SourceLocation' is what made @hypha symbol@ read
-- the file a second time under the wrong language settings.
data SymbolSite
  = ResolvedSite !LocatedDefinition
  | SweptSite    !SourceLocation
  deriving stock (Show, Eq)

siteLocation :: SymbolSite -> SourceLocation
siteLocation = \case
  ResolvedSite ld -> ldLocation ld
  SweptSite loc   -> loc

-- | Locate a symbol's definition inside its module.
--
-- With a parsable cabal file we resolve the symbol through the component's
-- exports, which is the only way to answer correctly for a re-export: the
-- module the user named does not declare the symbol, and the
-- package-wide sweep that used to fill that gap picks whichever
-- same-named binding it enumerates first — @Data.Map.Strict.insertWith@
-- came back as @Data\/IntMap\/Internal.hs@.  The sweep survives only for
-- packages whose cabal we cannot read, and says so.
--
-- The reach is how a re-export that leaves the package gets followed.  It
-- is built from the build plan (see "Hypha.Source.Dependencies"), which is
-- what @Data.List@ needs: since GHC 9.10 @base@ is a facade over
-- @ghc-internal@, so nearly every @base@ symbol is defined in another
-- package.  A caller with no plan gets an empty reach — the plan-less path
-- reports the re-export it cannot follow rather than guessing at one.
locateSymbolSite
  :: BuildContext
     -- ^ The plan's CPP environment and platform, so this path reads a
     -- module the same way the indexer does.
  -> OutsideReach IO
  -> PackageId
  -> FilePath
  -> ModulePath
  -> SymbolName
  -> IO (Either SymbolSearchFailure SymbolSite)
locateSymbolSite ctx reach pid srcDir asking sym = do
  comps <- Indexer.packageSources srcDir ctx
  -- The component key comes from the 'PackageId' the caller already
  -- holds.  Deriving it from the cabal file's basename was a second,
  -- weaker answer to a question that was already settled -- and for a
  -- store path with no cabal file it produced @containers-0.6.7@ as the
  -- package name.
  let compKey = componentKeyOf (pkgName pid)
      matching =
        [ (Comp.ciLanguageSettings ci, Comp.ciKind ci, sources)
        | (ci, sources) <- comps
        , any ((== asking) . msDeclaredName) sources
        ]
  case matching of
    ((langs, kind, sources) : _) ->
      fmap ResolvedSite
        <$> locateDefinitionInComponent langs (compKey kind) sources
              reach asking sym
    -- The scan is for a module we cannot resolve, never for one the
    -- package does not have.  Ranking every same-named binding in the
    -- tree by shared path suffix will always find *something*:
    -- @containers\/Data.Map.Strct\/insertWith@ (one letter missing) came
    -- back as the @IntMap@ function, exit 0, under the misspelled module's
    -- own name — a confident wrong answer where the predecessor of this
    -- code path had correctly said the module was not there.
    --
    -- A module the components do not list but whose file exists is a real
    -- case — generated modules, CPP-selected platform variants — and stays
    -- scannable.  Both readable-cabal cases are distinguished from "this
    -- package has no library stanza at all", which is the one the scan was
    -- introduced for.
    [] -> do
      mFile <- findModuleFile srcDir (unModulePath asking)
      case (comps, mFile) of
        (_ : _, Nothing) -> pure (Left SearchModuleAbsent)
        _                -> do
          hPutStrLn stderr $
            "hypha: no cabal component of " <> srcDir <> " lists "
              <> Text.unpack (unModulePath asking)
              <> "; falling back to a package scan"
          swept <- locateSymbolDefinitionInDir srcDir
                     (unModulePath asking) (unSymbolName sym)
          pure $ case swept of
            Just loc -> Right (SweptSite loc)
            Nothing  -> Left (SearchSweptPackage srcDir)

-- | The error a failed location becomes, with the reach's own gaps folded
-- in.
--
-- The gaps are read here and only here: at the moment a failure is
-- reported, which is the only moment they explain anything.  Warning about
-- them as they happen is what attached @Hackage HTTP 404 for \'rts\'@ to a
-- query that had already answered correctly.
symbolNotFound
  :: OutsideReach IO
  -> PackageId
  -> ModulePath
  -> SymbolName
  -> SymbolSearchFailure
  -> IO HyphaError
symbolNotFound reach pid asking sym failure = do
  gaps <- orGaps reach
  pure (NotFound (NotFoundSymbol pid asking sym failure gaps))

-- | Lift a 'Maybe' into 'ExceptT' with a typed error on 'Nothing'.
liftMaybe :: Monad m => HyphaError -> Maybe a -> ExceptT HyphaError m a
liftMaybe err = maybe (throwE err) pure

-- | The source a span covers, and the span actually taken — clamped to
-- the file, so the reported bounds are what the caller received rather
-- than what was asked for.
sliceSpan :: SpanKind -> (Int, Int) -> Text -> (Text, SnippetSpan)
sliceSpan kind (wantStart, wantEnd) content =
  let allLines = Text.lines content
      total    = length allLines
      start    = max 1 (min wantStart total)
      end      = max start (min wantEnd total)
      taken    = take (end - start + 1) (drop (start - 1) allLines)
  in (Text.unlines taken, SnippetSpan kind start end)

-- | The fallback when no span is known: fifteen lines either side of the
-- target.  Reported as 'SpanWindow', never as a declaration.
windowAround :: Int -> Text -> (Text, SnippetSpan)
windowAround target =
  sliceSpan SpanWindow (target - 15, target + 15)

-- | Convert a source result to JSON.
sourceResultToJSON :: SourceResult -> Value
sourceResultToJSON sr = Aeson.object $ concat
  [ [ "package" .= srcPackage sr
    , "version" .= srcVersion sr
    , "module"  .= srcModule sr
    ]
  , maybe [] (\s -> ["symbol" .= s]) (srcSymbol sr)
  , [ "path"    .= srcPath sr
    , "line"    .= srcLine sr
    , "snippet" .= srcSnippet sr
    , "span"    .= Aeson.object
        [ "kind"  .= spanKindText (ssKind (srcSpan sr))
        , "start" .= ssStart (srcSpan sr)
        , "end"   .= ssEnd (srcSpan sr)
        ]
    ]
  , [ "defined_in" .= Aeson.object
        [ "module"    .= unModulePath (diModule d)
        , "component" .= unComponentKey (diComponent d)
        ]
    | Just d <- [srcDefinedIn sr]
    ]
  ]

