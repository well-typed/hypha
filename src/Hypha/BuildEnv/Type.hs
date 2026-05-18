module Hypha.BuildEnv.Type
  ( BuildEnv (..)
  ) where

import Data.Set (Set)
import Hypha.Types.PackageId (PackageId, Version)

data BuildEnv m = BuildEnv
  { discoverInstalledPackages :: !(m (Set PackageId))
  , locatePackageSource       :: !(PackageId -> m (Maybe FilePath))
  , locateHaddockHtml         :: !(PackageId -> m (Maybe FilePath))
  , ghcVersion                :: !(m Version)
  }
