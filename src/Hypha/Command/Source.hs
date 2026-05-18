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
runSource :: BuildEnv IO -> BuildPlan -> PackageId -> Text -> Maybe Text -> IO (Either HyphaError (Outcome Value))
runSource env _plan pid modPath mSym = do
  mSrcDir <- locatePackageSource env pid
  case mSrcDir of
    Nothing -> pure (Left $ NotFound
      ("source not found for " <> renderPid pid <> "; run `cabal build` first"))
    Just srcDir -> runSourceFromDir env pid srcDir modPath mSym

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
runSourceFromDir _env pid srcDir modPath mSym = do
  mFile <- findModuleFile srcDir modPath
  case mFile of
    Nothing -> pure (Left $ NotFound
      ("module file not found under " <> Text.pack srcDir
        <> " for " <> modPath))
    Just filePath -> do
      mLoc <- case mSym of
        Nothing  -> pure (Just (SourceLocation filePath 1))
        Just sym -> locateSymbolDefinitionInDir srcDir modPath sym
      case mLoc of
        Nothing -> pure (Left $ NotFound
          ("symbol '" <> fromMaybe "" mSym <> "' not found in " <> modPath))
        Just loc -> do
          content <- TIO.readFile (slPath loc)
          let allLines = Text.lines content
              targetLine = slLine loc
              snippet = extractSnippet targetLine allLines
              result = SourceResult
                { srcPackage = unPackageName (pkgName pid)
                , srcVersion = unVersion (pkgVersion pid)
                , srcModule  = modPath
                , srcSymbol  = mSym
                , srcPath    = slPath loc
                , srcLine    = slLine loc
                , srcSnippet = snippet
                }
          pure (Right $ successOutcome (sourceResultToJSON result))

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

-- Helper
fromMaybe :: a -> Maybe a -> a
fromMaybe d Nothing  = d
fromMaybe _ (Just x) = x
