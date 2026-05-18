{-# LANGUAGE DerivingStrategies #-}
module Hypha.Cache
  ( cacheRoot
  , hackageCacheDir
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
