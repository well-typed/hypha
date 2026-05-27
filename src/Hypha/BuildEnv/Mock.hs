{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
module Hypha.BuildEnv.Mock
  ( -- * Types
    MockBuildEnv (..)
  , emptyMock
    -- * Construction
  , mkMockBuildEnv
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Hypha.BuildEnv.Type (BuildEnv (..))
import Hypha.Types.PackageId (PackageId (..), Version (..))

-- | Configuration for a mock build environment.
data MockBuildEnv = MockBuildEnv
  { mockPackages     :: !(Map PackageId (Maybe FilePath, Maybe FilePath))
    -- ^ @(sourceDir, haddockHtml)@ for each installed package.
  , mockGhcVersion   :: !Version
    -- ^ The GHC version to report.
  }
  deriving stock (Show)

-- | An empty mock build environment with no packages and GHC version "0.0.0".
emptyMock :: MockBuildEnv
emptyMock = MockBuildEnv
  { mockPackages   = Map.empty
  , mockGhcVersion = Version "0.0.0"
  }

-- | Create a 'BuildEnv' from a 'MockBuildEnv' configuration.
--
--   Polymorphic in the carrier monad: tests can instantiate at 'Identity'
--   for pure assertions or 'IO' / 'State' as needed.  No IO is performed.
mkMockBuildEnv :: Applicative m => MockBuildEnv -> BuildEnv m
mkMockBuildEnv mock = BuildEnv
  { discoverInstalledPackages = pure (Map.keysSet (mockPackages mock))
  , locatePackageSource       = \pkgId ->
      pure (fst =<< Map.lookup pkgId (mockPackages mock))
  , locateRepoTarball         = \_ -> pure Nothing
  , locateHaddockHtml         = \pkgId ->
      pure (snd =<< Map.lookup pkgId (mockPackages mock))
  , ghcVersion                = pure (mockGhcVersion mock)
  }
