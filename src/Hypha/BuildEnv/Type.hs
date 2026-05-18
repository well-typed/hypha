{-# LANGUAGE DerivingStrategies #-}
module Hypha.BuildEnv.Type
  ( BuildEnv (..)
  ) where

import Data.Set (Set)

import Hypha.Types.PackageId (PackageId, Version)

-- | Record-of-functions interface for interacting with the build environment.
--
--   Parameterized over @m@ so production code uses @IO@ and tests can use
--   pure effect carriers like @Identity@ or @State MockState@.
data BuildEnv m = BuildEnv
  { discoverInstalledPackages :: !(m (Set PackageId))
    -- ^ Enumerate all installed packages in the build environment.
  , locatePackageSource       :: !(PackageId -> m (Maybe FilePath))
    -- ^ Find the source directory for a given package.
  , locateHaddockHtml         :: !(PackageId -> m (Maybe FilePath))
    -- ^ Find the rendered Haddock HTML for a given package.
  , ghcVersion                :: !(m Version)
    -- ^ Get the GHC version of the build environment.
  }
