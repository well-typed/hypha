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
  ) where

import Data.Aeson (Value (..), (.=))
import qualified Data.Aeson as Aeson
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import System.Directory (doesFileExist)
import System.FilePath ((</>))

import Hypha.BuildEnv.Type (BuildEnv (..))
import Hypha.Error (HyphaError (..))
import Hypha.Output.Outcome (Outcome, successOutcome)
import Hypha.Source.Locate (SourceLocation (..), locateSymbolDefinition, modulePathToFile)
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
  -- Try to locate the source file
  mSrcDir <- locatePackageSource env pid
  case mSrcDir of
    Nothing -> pure (Left $ NotFound
      ("source not found for " <> renderPid pid <> "; run `cabal build` first"))
    Just srcDir -> do
      let filePath = srcDir </> modulePathToFile modPath
      exists <- doesFileExist filePath
      if not exists
        then pure (Left $ NotFound
          ("module file not found: " <> Text.pack filePath))
        else do
          -- Find the symbol location (or use line 1 for module header)
          mLoc <- case mSym of
            Nothing -> pure (Just (SourceLocation filePath 1))
            Just sym -> locateSymbolDefinition env pid modPath sym

          case mLoc of
            Nothing -> pure (Left $ NotFound
              ("symbol '" <> fromMaybe "" mSym <> "' not found in " <> modPath))
            Just loc -> do
              -- Read the file and extract a 30-line snippet
              content <- TIO.readFile (slPath loc)
              let allLines = Text.lines content
                  targetLine = slLine loc
                  snippet = extractSnippet targetLine allLines

              let result = SourceResult
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
