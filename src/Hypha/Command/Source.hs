{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Command.Source
  ( -- * Types
    SourceResult (..)
    -- * Field sets
  , compactKeys
  , fullKeys
    -- * Execution
  , runSource
  , runSourceFromDir
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
import Hypha.Search.Index (ModuleSource (..), noImportedDefinitions)
import Hypha.Search.Indexer qualified as Indexer
import Hypha.Types.ComponentName (componentKeyOf)
import Hypha.Source.Locate
  ( LocatedDefinition (..), SourceLocation (..), findModuleFile
  , locateDefinitionInComponent, locateSymbolDefinitionInDir )
import Hypha.Types.SymbolPath (ModulePath (..), SymbolName (..))
import System.IO (hPutStrLn, stderr)
import Hypha.Types.BuildPlan (BuildPlan (..))
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
  }
  deriving stock (Show)

-- | Compact field set.
compactKeys :: Set Text
compactKeys = Set.fromList
  [ "package", "module", "symbol", "path", "line", "snippet" ]

-- | Full field set.
fullKeys :: Set Text
fullKeys = Set.fromList
  [ "package", "version", "module", "symbol", "path", "line", "snippet" ]

-- | Execute the source command.
--
--   Parses the argument as @PKG/MOD[/SYM]@ and returns a 30-line snippet
--   around the symbol definition (or the module header if no symbol).
runSource
  :: BuildEnv IO -> BuildPlan -> PackageId -> Text -> Maybe Text
  -> IO (Either HyphaError (Outcome Value))
runSource env _plan pid modPath mSym = runExceptT $ do
  srcDir <- liftMaybe (NotFound (NotFoundSource pid))
    =<< liftIO (locatePackageSource env pid)
  sourceFromDirE pid srcDir modPath mSym

-- | Variant that takes an already-resolved source directory.  Used by the
-- 'PackageResolver'-driven dispatch path so the full fallback chain (plan
-- → store → Hackage tarball) can locate sources before this command runs.
runSourceFromDir
  :: BuildEnv IO
  -> PackageId
  -> FilePath   -- ^ Source directory (resolved upstream).
  -> Text       -- ^ Module path (dotted).
  -> Maybe Text -- ^ Optional symbol name.
  -> IO (Either HyphaError (Outcome Value))
runSourceFromDir _env pid srcDir modPath mSym =
  runExceptT (sourceFromDirE pid srcDir modPath mSym)

-- | Shared ExceptT body: find the module file, locate the (optional)
-- symbol, then build the snippet.
sourceFromDirE
  :: PackageId -> FilePath -> Text -> Maybe Text
  -> ExceptT HyphaError IO (Outcome Value)
sourceFromDirE pid srcDir modPath mSym = do
  -- Split, because the two cases fail differently and the merged version
  -- could build a 'NotFoundSymbol' carrying an empty symbol name -- a
  -- value no caller could ever produce.  It also stopped locating a
  -- module the component list would have found: 'findModuleFile' skips
  -- @test\/@ and @bench\/@, so a symbol in a test-suite module failed
  -- before the cabal-driven lookup ran at all.
  loc <- case mSym of
    Nothing -> do
      filePath <- liftMaybe
        (NotFound (NotFoundModuleFileUnder srcDir modPath))
        =<< liftIO (findModuleFile srcDir modPath)
      pure (SourceLocation filePath 1)
    Just sym -> liftMaybe
      (NotFound (NotFoundSymbol pid modPath sym))
      =<< liftIO (locateSymbolLoc pid srcDir modPath sym)
  content <- liftIO (TIO.readFile (slPath loc))
  let snippet = extractSnippet (slLine loc) (Text.lines content)
      result = SourceResult
        { srcPackage = unPackageName (pkgName pid)
        , srcVersion = unVersion (pkgVersion pid)
        , srcModule  = modPath
        , srcSymbol  = mSym
        , srcPath    = slPath loc
        , srcLine    = slLine loc
        , srcSnippet = snippet
        }
  pure (successOutcome SourceCmd (sourceResultToJSON result))

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
-- No imported modules are supplied: a bare package directory comes with no
-- build plan, so there is no dependency graph to find the owner of a
-- cross-package re-export in.  'locateDefinitionInComponent' reports those
-- rather than guessing — @hypha server@, which does have a plan, resolves
-- them.
locateSymbolLoc
  :: PackageId -> FilePath -> Text -> Text -> IO (Maybe SourceLocation)
locateSymbolLoc pid srcDir modPath sym = do
  comps <- Indexer.packageSources srcDir
  -- The component key comes from the 'PackageId' the caller already
  -- holds.  Deriving it from the cabal file's basename was a second,
  -- weaker answer to a question that was already settled -- and for a
  -- store path with no cabal file it produced @containers-0.6.7@ as the
  -- package name.
  let compKey = componentKeyOf (pkgName pid)
      matching =
        [ (Comp.ciLanguageSettings ci, Comp.ciKind ci, sources)
        | (ci, sources) <- comps
        , any ((== ModulePath modPath) . msDeclaredName) sources
        ]
  case matching of
    ((langs, kind, sources) : _) -> do
      mLd <- locateDefinitionInComponent langs (compKey kind) sources
               noImportedDefinitions (ModulePath modPath) (SymbolName sym)
      case mLd of
        Just ld -> pure (Just (ldLocation ld))
        Nothing -> pure Nothing
    [] -> do
      hPutStrLn stderr $
        "hypha: no cabal component of " <> srcDir <> " lists "
          <> Text.unpack modPath <> "; falling back to a package scan"
      locateSymbolDefinitionInDir srcDir modPath sym

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
  ]

