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
  { ppVersion  :: !Version
  , ppIsLocal  :: !Bool
    -- ^ Whether the package is a local project package (not a dependency).
  , ppDeps     :: !Int
    -- ^ Number of library-level dependencies.
  }
  deriving stock (Show, Eq, Ord)

-- | The resolved build plan: pinned package versions from @plan.json@.
data BuildPlan = BuildPlan
  { bpCompiler  :: !CompilerId
  , bpPackages  :: !(Map PackageName PlanPackage)
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

-- | Look up a package in the plan.
lookupPackage :: PackageName -> BuildPlan -> Maybe PlanPackage
lookupPackage name = Map.lookup name . bpPackages

-- | All packages in the plan, as a list.
planPackages :: BuildPlan -> [PlanPackage]
planPackages bp = Map.elems (bpPackages bp)

-- | Apply overrides to a build plan.
--   Each override replaces (or inserts) the pinned version for its package.
--   The overridden entry keeps its original local/deps metadata if present,
--   or defaults to non-local with zero deps.
applyOverrides :: [PackageOverride] -> BuildPlan -> BuildPlan
applyOverrides overrides bp = bp
  { bpPackages  = foldr applyOverride (bpPackages bp) overrides
  , bpOverrides = overrides ++ bpOverrides bp
  }
  where
    applyOverride :: PackageOverride -> Map PackageName PlanPackage -> Map PackageName PlanPackage
    applyOverride (PackageOverride n v) m =
      let existing = Map.lookup n m
          pp = PlanPackage
            { ppVersion  = v
            , ppIsLocal  = maybe False ppIsLocal existing
            , ppDeps     = maybe 0    ppDeps    existing
            }
      in Map.insert n pp m
