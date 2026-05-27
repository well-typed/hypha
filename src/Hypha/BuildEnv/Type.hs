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
    -- ^ Find the unpacked source directory for a given package.
  , locateRepoTarball         :: !(PackageId -> m (Maybe FilePath))
    -- ^ Find the path to a downloaded but unextracted @.tar.gz@ for a
    -- given package, as kept by cabal-install under
    -- @<packagesRoot>/<repo>/<pkg>/<ver>/<pkg>-<ver>.tar.gz@.  Returns
    -- 'Nothing' when no repository in the environment has the tarball
    -- (or when the environment has no notion of a repo cache, e.g.
    -- Nix).
  , locateHaddockHtml         :: !(PackageId -> m (Maybe FilePath))
    -- ^ Find the rendered Haddock HTML for a given package.
  , ghcVersion                :: !(m Version)
    -- ^ Get the GHC version of the build environment.
  }
