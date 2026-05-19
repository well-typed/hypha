{-# LANGUAGE DerivingStrategies #-}
module Hypha.Cache
  ( cacheRoot
  , hackageCacheDir
  , haddockCacheRoot
  , sourceCacheRoot
  ) where

import System.Directory (getXdgDirectory, XdgDirectory (XdgCache))
import System.FilePath ((</>))

-- | Root cache directory for hypha.
-- Follows XDG: @${XDG_CACHE_HOME:-~/.cache}/hypha/@
cacheRoot :: IO FilePath
cacheRoot = getXdgDirectory XdgCache "hypha"

-- | Hackage HTTP cache subdirectory.
hackageCacheDir :: IO FilePath
hackageCacheDir = (</> "hackage") <$> cacheRoot

-- | Haddock HTML cache subdirectory.
haddockCacheRoot :: IO FilePath
haddockCacheRoot = (</> "haddock") <$> cacheRoot

-- | Extracted package-source cache subdirectory.  Each subdirectory is
-- named @\"\<pkg\>-\<ver\>\"@ and holds the package's source tree.
sourceCacheRoot :: IO FilePath
sourceCacheRoot = (</> "source") <$> cacheRoot
