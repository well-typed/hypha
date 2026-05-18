{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Source.Locate
  ( listExportedSymbols
  , locateSymbolDefinition
  , SourceLocation (..)
    -- * Testing
  , parseExports
  , modulePathToFile
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import System.Directory (doesFileExist)
import System.FilePath ((</>))

import Hypha.BuildEnv.Type   (BuildEnv (..))
import Hypha.Types.PackageId (PackageId (..))

-- | Location of a symbol definition in a source file.
data SourceLocation = SourceLocation
  { slPath :: !FilePath
  , slLine :: !Int
  }
  deriving stock (Show, Eq)

-- | Inspect the package source directory and list candidate exported symbols
-- by scraping the @module ... ( ... ) where@ header naïvely.
--
-- A future improvement (post-MVP) is to use ghc-lib-parser for accurate parsing.
listExportedSymbols :: BuildEnv IO -> PackageId -> Text -> IO [Text]
listExportedSymbols env pid modPath = do
  mDir <- locatePackageSource env pid
  case mDir of
    Nothing -> pure []
    Just d  -> do
      let f = d </> modulePathToFile modPath
      ok <- doesFileExist f
      if not ok
        then pure []
        else parseExports <$> TIO.readFile f

-- | Convert a module path like @Control.Concurrent.Async@ to a file path
-- like @Control/Concurrent/Async.hs@.
modulePathToFile :: Text -> FilePath
modulePathToFile m = Text.unpack (Text.replace "." "/" m) <> ".hs"

-- | Crude header parser: take everything between the first @(@ and the matching @)@.
parseExports :: Text -> [Text]
parseExports src =
  let body = Text.dropWhile (/= '(') src
      end  = Text.takeWhile (/= ')') (Text.drop 1 body)
      raw  = Text.splitOn "," end
  in [ trim x | x <- raw, not (Text.null (trim x)) ]
  where
    trim :: Text -> Text
    trim = Text.dropWhile (`elem` (" \t\n" :: String))
         . Text.dropWhileEnd (`elem` (" \t\n" :: String))

-- | Find the line where a symbol is defined.
locateSymbolDefinition :: BuildEnv IO -> PackageId -> Text -> Text -> IO (Maybe SourceLocation)
locateSymbolDefinition env pid modPath sym = do
  mDir <- locatePackageSource env pid
  case mDir of
    Nothing -> pure Nothing
    Just d  -> do
      let f = d </> modulePathToFile modPath
      ok <- doesFileExist f
      if not ok
        then pure Nothing
        else do
          ls <- Text.lines <$> TIO.readFile f
          pure $ case [ i | (i, l) <- zip [1 :: Int ..] ls, startsWith sym l ] of
                   (i:_) -> Just (SourceLocation f i)
                   []    -> Nothing
  where
    startsWith name l =
      let trimmed = Text.dropWhile (== ' ') l
      in name `Text.isPrefixOf` trimmed
