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
import Hypha.Types.PackageId (PackageId (..), renderPackageId)

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
-- 2. Fall back to the build environment's store location.
-- 3. Return 'Nothing' if neither has it (best-effort build is future
--    work; callers should treat this as "no docs available yet").
ensureHaddockFor :: BuildEnv IO -> PackageId -> IO (Maybe FilePath)
ensureHaddockFor env pid = do
  inCache <- haddockCacheExists pid
  if inCache
    then do
      dir  <- haddockDirFor pid
      let idx = dir </> "index.html"
      ok <- doesFileExist idx
      pure (if ok then Just idx else Nothing)
    else locateHaddockHtml env pid
