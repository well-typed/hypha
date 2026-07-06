{-# LANGUAGE DerivingStrategies #-}
module Hypha.Cache
  ( cacheRoot
  , hackageCacheDir
  , haddockCacheRoot
  , sourceCacheRoot
  ) where

import System.Directory (getXdgDirectory, XdgDirectory (XdgCache))
import System.FilePath ((</>))

-- | Root cache directory for hypha (production).
-- Follows XDG: @${XDG_CACHE_HOME:-~/.cache}/hypha/@
-- Only called by 'runHypha' to resolve 'heCacheDir'; everything else
-- receives the resolved path from the environment.
cacheRoot :: IO FilePath
cacheRoot = getXdgDirectory XdgCache "hypha"

-- | Hackage HTTP cache subdirectory.
hackageCacheDir :: FilePath -> FilePath
hackageCacheDir root = root </> "hackage"

-- | Haddock HTML cache subdirectory.
haddockCacheRoot :: FilePath -> FilePath
haddockCacheRoot root = root </> "haddock"

-- | Extracted package-source cache subdirectory.  Each subdirectory is
-- named @\"\<pkg\>-\<ver\>\"@ and holds the package's source tree.
sourceCacheRoot :: FilePath -> FilePath
sourceCacheRoot root = root </> "source"