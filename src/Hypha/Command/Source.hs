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
import Data.Aeson (Value (..), (.=))
import qualified Data.Aeson as Aeson
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO

import Hypha.BuildEnv.Type (BuildEnv (..))
import Hypha.Error (HyphaError (..))
import Hypha.Output.Outcome (Outcome, successOutcome)
import Hypha.Source.Locate
  ( SourceLocation (..), findModuleFile, locateSymbolDefinitionInDir )
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
  srcDir <- liftMaybe
    (NotFound ("source not found for " <> renderPid pid
               <> "; run `cabal build` first"))
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
  filePath <- liftMaybe
    (NotFound ("module file not found under " <> Text.pack srcDir
               <> " for " <> modPath))
    =<< liftIO (findModuleFile srcDir modPath)
  loc <- liftMaybe
    (NotFound ("symbol '" <> maybe "" id mSym
               <> "' not found in " <> modPath))
    =<< liftIO (locateSourceLoc filePath srcDir modPath mSym)
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
  pure (successOutcome (sourceResultToJSON result))

-- | When a symbol is provided, locate its definition inside the module;
-- otherwise pin to line 1 of the resolved module file.
locateSourceLoc
  :: FilePath -> FilePath -> Text -> Maybe Text -> IO (Maybe SourceLocation)
locateSourceLoc filePath _      _       Nothing    = pure (Just (SourceLocation filePath 1))
locateSourceLoc _        srcDir modPath (Just sym) = locateSymbolDefinitionInDir srcDir modPath sym

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

-- Helper
renderPid :: PackageId -> Text
renderPid (PackageId (PackageName n) (Version v)) = n <> "-" <> v
