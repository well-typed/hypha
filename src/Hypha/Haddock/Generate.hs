{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Haddock.Generate
  ( haddockDirFor
  , haddockCacheExists
  , ensureHaddockFor
  ) where

import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath ((</>))
import qualified Data.Text as Text

import Hypha.BuildEnv.Type (BuildEnv (..))
import Hypha.Cache (haddockCacheRoot)
import Hypha.Types.BuildPlan (BuildPlan, PlannedUnit (..), lookupUnit)
import Hypha.Types.PackageId (PackageId (..), PackageName (..), renderPackageId)

-- | Return the per-package Haddock cache directory.
--
-- Layout: @<cacheRoot>/haddock/<pkg>-<ver>/@
haddockDirFor :: FilePath -> PackageId -> FilePath
haddockDirFor cacheRoot pid =
  haddockCacheRoot cacheRoot </> Text.unpack (renderPackageId pid)

-- | Check whether the Haddock cache directory for a package already
-- exists.
haddockCacheExists :: FilePath -> PackageId -> IO Bool
haddockCacheExists cacheRoot pid =
  doesDirectoryExist (haddockDirFor cacheRoot pid)

-- | Ensure rendered Haddock HTML is available for a package.
--
-- Resolution order:
--
-- 1. Check the hypha Haddock cache (@<cacheRoot>/haddock/...@).
-- 2. For local packages, check the build plan's dist-dir (NEW).
-- 3. Fall back to the build environment's store location.
-- 4. Return 'Nothing' if none has it.
ensureHaddockFor :: FilePath -> BuildPlan -> BuildEnv IO -> PackageId -> IO (Maybe FilePath)
ensureHaddockFor cacheRoot plan env pid = do
  inCache <- haddockCacheExists cacheRoot pid
  if inCache
    then do
      let dir  = haddockDirFor cacheRoot pid
          idx = dir </> "index.html"
      ok <- doesFileExist idx
      pure (if ok then Just idx else Nothing)
    else do
      mDist <- distDirHaddock plan pid
      case mDist of
        Just idx -> pure (Just idx)
        Nothing  -> locateHaddockHtml env pid

-- | Check the build plan's dist-dir for a pre-built Haddock HTML index.
-- This is used for local (inplace) packages whose Haddock is under
-- @<distDir>/doc/html/<pkg>/index.html@ (the standard cabal-install layout).
distDirHaddock :: BuildPlan -> PackageId -> IO (Maybe FilePath)
distDirHaddock plan pid =
  case lookupUnit (pkgName pid) plan of
    Just pu | Just d <- puDistDir pu -> do
      let pkgNameStr = Text.unpack (unPackageName (pkgName pid))
          idx = d </> "doc" </> "html" </> pkgNameStr </> "index.html"
      ok <- doesFileExist idx
      if ok then pure (Just idx) else pure Nothing
    _ -> pure Nothing
