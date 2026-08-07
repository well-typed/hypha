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
import Data.Text.IO qualified as TIO
import Data.Text qualified as Text
import Data.Text (Text)
import Hypha.BuildEnv.Type (BuildEnv (..))
import Hypha.Cli.Types
import Hypha.Error (HyphaError (..), NotFoundReason (..))
import Hypha.Output.Outcome (Outcome, successOutcome)
import Hypha.Project.Components qualified as Comp
import Hypha.Search.Index (ModuleSource (..))
import Hypha.Search.Indexer qualified as Indexer
import Hypha.Types.ComponentName
  (ComponentKey (..), componentKeyOf)
import Hypha.Source.Locate
  ( LocatedDefinition (..), SourceLocation (..), findModuleFile
  , locateDefinitionInComponent, locateSymbolDefinitionInDir )
import Hypha.Source.Reach
  ( OutsideReach (..), SymbolSearchFailure (..) )
import Hypha.Types.SymbolPath (ModulePath (..), SymbolName (..))
import System.IO (hPutStrLn, stderr)
import Hypha.Types.PackageId (PackageId (..), PackageName (..), Version (..))

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
    -- ^ 30-line source snippet around the definition.
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
  [ "package", "module", "symbol", "path", "line", "snippet"
  -- In the compact set because the compact set is what an agent reads by
  -- default, and a path in another package beside an unannotated module
  -- name is precisely where it would be misled.
  , "defined_in" ]

-- | Full field set.
fullKeys :: Set Text
fullKeys = Set.fromList
  [ "package", "version", "module", "symbol", "path", "line", "snippet"
  , "defined_in" ]

-- | Execute the source command.
--
--   Parses the argument as @PKG/MOD[/SYM]@ and returns a 30-line snippet
--   around the symbol definition (or the module header if no symbol).
runSource
  :: Maybe FilePath -> BuildEnv IO -> OutsideReach IO -> PackageId -> Text
  -> Maybe Text
  -> IO (Either HyphaError (Outcome Value))
runSource mMacroHeader env reach pid modPath mSym = runExceptT $ do
  srcDir <- liftMaybe (NotFound (NotFoundSource pid))
    =<< liftIO (locatePackageSource env pid)
  sourceFromDirE mMacroHeader reach pid srcDir modPath mSym

-- | Variant that takes an already-resolved source directory.  Used by the
-- 'PackageResolver'-driven dispatch path so the full fallback chain (plan
-- → store → Hackage tarball) can locate sources before this command runs.
runSourceFromDir
  :: Maybe FilePath
    -- ^ The plan's synthesised @cabal_macros.h@.
  -> OutsideReach IO
    -- ^ The dependency closure, for a re-export that leaves the package.
  -> PackageId
  -> FilePath   -- ^ Source directory (resolved upstream).
  -> Text       -- ^ Module path (dotted).
  -> Maybe Text -- ^ Optional symbol name.
  -> IO (Either HyphaError (Outcome Value))
runSourceFromDir mMacroHeader reach pid srcDir modPath mSym =
  runExceptT (sourceFromDirE mMacroHeader reach pid srcDir modPath mSym)

-- | Shared ExceptT body: find the module file, locate the (optional)
-- symbol, then build the snippet.
sourceFromDirE
  :: Maybe FilePath -> OutsideReach IO -> PackageId -> FilePath -> Text
  -> Maybe Text
  -> ExceptT HyphaError IO (Outcome Value)
sourceFromDirE mMacroHeader reach pid srcDir modPath mSym = do
  -- Split, because the two cases fail differently and the merged version
  -- could build a 'NotFoundSymbol' carrying an empty symbol name -- a
  -- value no caller could ever produce.  It also stopped locating a
  -- module the component list would have found: 'findModuleFile' skips
  -- @test\/@ and @bench\/@, so a symbol in a test-suite module failed
  -- before the cabal-driven lookup ran at all.
  site <- case mSym of
    Nothing -> do
      filePath <- liftMaybe
        (NotFound (NotFoundModuleFileUnder srcDir modPath))
        =<< liftIO (findModuleFile srcDir modPath)
      pure (SweptSite (SourceLocation filePath 1))
    Just sym -> do
      located <- liftIO
        (locateSymbolSite mMacroHeader reach pid srcDir
           (ModulePath modPath) (SymbolName sym))
      case located of
        Right s  -> pure s
        Left err -> throwE
          =<< liftIO (symbolNotFound reach pid (ModulePath modPath)
                        (SymbolName sym) err)
  let loc = siteLocation site
  content <- liftIO (TIO.readFile (slPath loc))
  let snippet = extractSnippet (slLine loc) (Text.lines content)
      result = SourceResult
        { srcPackage   = unPackageName (pkgName pid)
        , srcVersion   = unVersion (pkgVersion pid)
        , srcModule    = modPath
        , srcSymbol    = mSym
        , srcPath      = slPath loc
        , srcLine      = slLine loc
        , srcSnippet   = snippet
        , srcDefinedIn = definedElsewhere (ModulePath modPath) site
        }
  pure (successOutcome SourceCmd (sourceResultToJSON result))

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
  :: Maybe FilePath
     -- ^ The plan's synthesised @cabal_macros.h@, so this path
     -- preprocesses a module the same way the indexer does.
  -> OutsideReach IO
  -> PackageId
  -> FilePath
  -> ModulePath
  -> SymbolName
  -> IO (Either SymbolSearchFailure SymbolSite)
locateSymbolSite mMacroHeader reach pid srcDir asking sym = do
  comps <- Indexer.packageSources srcDir mMacroHeader
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

-- | Extract a 30-line snippet around the target line (15 lines before, 15 after).
extractSnippet :: Int -> [Text] -> Text
extractSnippet targetLine allLines =
  let totalLines = length allLines
      start = max 1 (targetLine - 15) - 1  -- 0-indexed
      end = min totalLines (targetLine + 15)
      snippetLines = take (end - start) (drop start allLines)
  in Text.unlines snippetLines

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
    ]
  , [ "defined_in" .= Aeson.object
        [ "module"    .= unModulePath (diModule d)
        , "component" .= unComponentKey (diComponent d)
        ]
    | Just d <- [srcDefinedIn sr]
    ]
  ]

