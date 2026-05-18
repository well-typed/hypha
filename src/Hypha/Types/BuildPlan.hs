{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Types.BuildPlan
  ( -- * Core types
    BuildPlan (..)
  , PlanPackage (..)
  , ProjectRoot (..)
  , CompilerId (..)
  , PlanHash (..)
  , PackageOverride (..)
    -- * Construction
  , emptyBuildPlan
    -- * Queries
  , lookupPackage
  , planPackages
  , applyOverrides
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

import Hypha.Types.PackageId (PackageName (..), Version (..))

-- | Absolute path to the project root (directory containing @cabal.project@).
newtype ProjectRoot = ProjectRoot { unProjectRoot :: FilePath }
  deriving stock   (Show, Eq, Ord)
  deriving newtype (Read)

-- | GHC compiler identifier, e.g. @ghc-9.6.7@.
newtype CompilerId = CompilerId { unCompilerId :: Text }
  deriving stock   (Show, Eq, Ord)
  deriving newtype (Read)

-- | Hash of @plan.json@ used to detect staleness.
newtype PlanHash = PlanHash { unPlanHash :: Text }
  deriving stock   (Show, Eq, Ord)
  deriving newtype (Read)

-- | A user-supplied version override, e.g. @async=2.2.6@.
data PackageOverride = PackageOverride
  { poName    :: !PackageName
  , poVersion :: !Version
  }
  deriving stock (Show, Eq, Ord)

-- | A single package entry in the build plan.
data PlanPackage = PlanPackage
  { ppName    :: !PackageName
  , ppVersion :: !Version
  }
  deriving stock (Show, Eq, Ord)

-- | The resolved build plan: pinned package versions from @plan.json@.
data BuildPlan = BuildPlan
  { bpCompiler  :: !CompilerId
  , bpPackages  :: !(Map PackageName Version)
  , bpOverrides :: ![PackageOverride]
  }
  deriving stock (Show)

-- | An empty build plan (no packages, no compiler).
emptyBuildPlan :: BuildPlan
emptyBuildPlan = BuildPlan
  { bpCompiler  = CompilerId "unknown"
  , bpPackages  = Map.empty
  , bpOverrides = []
  }

-- | Look up a package version in the plan.
lookupPackage :: PackageName -> BuildPlan -> Maybe Version
lookupPackage name = Map.lookup name . bpPackages

-- | All packages in the plan, as a list.
planPackages :: BuildPlan -> [PlanPackage]
planPackages bp =
  [ PlanPackage n v | (n, v) <- Map.toList (bpPackages bp) ]

-- | Apply overrides to a build plan.
--   Each override replaces (or inserts) the pinned version for its package.
applyOverrides :: [PackageOverride] -> BuildPlan -> BuildPlan
applyOverrides overrides bp = bp
  { bpPackages  = foldr (\(PackageOverride n v) -> Map.insert n v) (bpPackages bp) overrides
  , bpOverrides = overrides ++ bpOverrides bp
  }
