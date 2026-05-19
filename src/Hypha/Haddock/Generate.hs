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
-- Layout: @~/.cache/hypha/haddock/<pkg>-<ver>/@
haddockDirFor :: PackageId -> IO FilePath
haddockDirFor pid = do
  root <- haddockCacheRoot
  pure (root </> Text.unpack (renderPackageId pid))

-- | Check whether the Haddock cache directory for a package already
-- exists.
haddockCacheExists :: PackageId -> IO Bool
haddockCacheExists pid = do
  dir <- haddockDirFor pid
  doesDirectoryExist dir

-- | Ensure rendered Haddock HTML is available for a package.
--
-- Resolution order:
--
-- 1. Check the hypha Haddock cache (@~/.cache/hypha/haddock/...@).
-- 2. For local packages, check the build plan's dist-dir (NEW).
-- 3. Fall back to the build environment's store location.
-- 4. Return 'Nothing' if none has it.
ensureHaddockFor :: BuildPlan -> BuildEnv IO -> PackageId -> IO (Maybe FilePath)
ensureHaddockFor plan env pid = do
  inCache <- haddockCacheExists pid
  if inCache
    then do
      dir  <- haddockDirFor pid
      let idx = dir </> "index.html"
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
