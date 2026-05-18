{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
module Hypha.BuildEnv.Cabal
  ( -- * Types
    CabalStoreError (..)
    -- * Construction
  , mkCabalBuildEnv
  ) where

import Control.Exception (IOException, try)
import Data.List (find, isPrefixOf)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (takeFileName, (</>))

import Hypha.BuildEnv.Type (BuildEnv (..))
import Hypha.Types.PackageId (PackageName (..), Version (..), PackageId (..))

-- | Errors specific to the Cabal build environment.
data CabalStoreError
  = StoreNotFound !FilePath
    -- ^ The cabal store directory does not exist.
  | GhcVersionUnknown
    -- ^ Could not determine GHC version from the store path.
  deriving stock (Show, Eq)

-- | Create a 'BuildEnv' backed by the Cabal store at @~/.cabal/store/ghc-X.Y.Z/@.
--
--   The GHC version is extracted from the store directory name.
mkCabalBuildEnv :: FilePath -> IO (Either CabalStoreError (BuildEnv IO))
mkCabalBuildEnv storeRoot = do
  exists <- doesDirectoryExist storeRoot
  if exists
    then do
      ghcVer <- detectGhcVersion storeRoot
      case ghcVer of
        Nothing -> pure (Left GhcVersionUnknown)
        Just ver -> pure (Right BuildEnv
          { discoverInstalledPackages = discoverInStore storeRoot
          , locatePackageSource       = locateSource storeRoot
          , locateHaddockHtml         = locateHaddock storeRoot
          , ghcVersion                = pure ver
          })
    else pure (Left (StoreNotFound storeRoot))

-- | Detect GHC version from the store directory name.
--   Expects format: @ghc-X.Y.Z@
detectGhcVersion :: FilePath -> IO (Maybe Version)
detectGhcVersion storeRoot = do
  let dirname = takeFileName storeRoot
  case parseGhcDirName dirname of
    Nothing -> pure Nothing
    Just ver -> pure (Just (Version (Text.pack ver)))
  where
    parseGhcDirName :: String -> Maybe String
    parseGhcDirName name
      | take 4 name == "ghc-" = Just (drop 4 name)
      | otherwise = Nothing

-- | Discover all installed packages in the cabal store.
discoverInStore :: FilePath -> IO (Set PackageId)
discoverInStore storeRoot = do
  result <- try @IOException (listDirectory storeRoot)
  case result of
    Left _  -> pure Set.empty
    Right entries -> do
      let pkgIds = mapMaybe parseStoreEntry entries
      pure (Set.fromList pkgIds)

-- | Parse a store directory entry into a PackageId.
--   Store entries have format: @<pkg-name>-<version>-<hash>@
parseStoreEntry :: String -> Maybe PackageId
parseStoreEntry entry =
  let entryT = Text.pack entry
  in case Text.breakOnEnd "-" entryT of
       ("", _) -> Nothing
       (nameVerT, _hash) ->
         let nameVer = Text.init nameVerT  -- drop trailing separator
         in case Text.breakOnEnd "-" nameVer of
              ("", _) -> Nothing
              (nameT, verT) ->
                let name = Text.init nameT  -- drop trailing separator
                    ver  = verT
                in if Text.null name || Text.null ver
                   then Nothing
                   else Just (PackageId (PackageName name) (Version ver))

-- | Locate the source directory for a package.
--   In the cabal store, sources are typically not kept after building.
--   We look for @dist-newstyle/src/@ relative to the store root.
locateSource :: FilePath -> PackageId -> IO (Maybe FilePath)
locateSource storeRoot (PackageId (PackageName name) (Version ver)) = do
  let srcDir = storeRoot </> "src" </> (Text.unpack name <> "-" <> Text.unpack ver)
  exists <- doesDirectoryExist srcDir
  if exists
    then pure (Just srcDir)
    else pure Nothing

-- | Locate the Haddock HTML for a package.
--   Look in @share/doc/<pkg>-<ver>/index.html@ within the store entry.
locateHaddock :: FilePath -> PackageId -> IO (Maybe FilePath)
locateHaddock storeRoot (PackageId (PackageName name) (Version ver)) = do
  -- Find the store entry directory for this package
  result <- try @IOException (listDirectory storeRoot)
  case result of
    Left _ -> pure Nothing
    Right entries -> do
      let prefix = Text.unpack name <> "-" <> Text.unpack ver
          matchingEntry = find (isPrefixOf prefix) entries
      case matchingEntry of
        Nothing -> pure Nothing
        Just entry -> do
          let indexPath = storeRoot </> entry </> "share" </> "doc" </> (Text.unpack name <> "-" <> Text.unpack ver) </> "index.html"
          exists <- doesFileExist indexPath
          if exists
            then pure (Just indexPath)
            else pure Nothing
