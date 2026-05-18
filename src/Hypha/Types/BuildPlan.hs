{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Types.BuildPlan
  ( -- * Core types
    BuildPlan (..)
  , PlannedUnit (..)
  , PlanPackage (..)
  , ProjectRoot (..)
  , CompilerId (..)
  , PlanHash (..)
  , PackageOverride (..)
    -- * Construction
  , emptyBuildPlan
    -- * Queries
  , lookupPackage
  , lookupUnit
  , planPackages
  , applyOverrides
  , forwardDepsOf
  , reverseDepsOf
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

import Hypha.Types.PackageId (PackageName (..), PackageId (..), Version (..))

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

-- | A planned unit with its dependencies.
data PlannedUnit = PlannedUnit
  { puId   :: !PackageId
  , puDeps :: ![PackageId]
  }
  deriving stock (Show, Eq)

-- | The resolved build plan: pinned package versions from @plan.json@.
data BuildPlan = BuildPlan
  { bpCompiler  :: !CompilerId
  , bpUnits     :: !(Map PackageName PlannedUnit)
  , bpOverrides :: ![PackageOverride]
  }
  deriving stock (Show)

-- | An empty build plan (no packages, no compiler).
emptyBuildPlan :: BuildPlan
emptyBuildPlan = BuildPlan
  { bpCompiler  = CompilerId "unknown"
  , bpUnits     = Map.empty
  , bpOverrides = []
  }

-- | Look up a package version in the plan.
lookupPackage :: PackageName -> BuildPlan -> Maybe Version
lookupPackage name bp = pkgVersion . puId <$> Map.lookup name (bpUnits bp)

-- | Look up a planned unit in the plan.
lookupUnit :: PackageName -> BuildPlan -> Maybe PlannedUnit
lookupUnit name = Map.lookup name . bpUnits

-- | All packages in the plan, as a list.
planPackages :: BuildPlan -> [PlanPackage]
planPackages bp =
  [ PlanPackage (pkgName (puId u)) (pkgVersion (puId u))
  | u <- Map.elems (bpUnits bp)
  ]

-- | Apply overrides to a build plan.
--   Each override replaces (or inserts) the pinned version for its package.
applyOverrides :: [PackageOverride] -> BuildPlan -> BuildPlan
applyOverrides overrides bp = bp
  { bpUnits     = foldr applyOverride (bpUnits bp) overrides
  , bpOverrides = overrides ++ bpOverrides bp
  }
  where
    applyOverride (PackageOverride n v) =
      Map.insertWith (\_ old -> old { puId = (puId old) { pkgVersion = v } }) n
        PlannedUnit { puId = PackageId n v, puDeps = [] }

-- | Get forward dependencies of a package.
forwardDepsOf :: PackageName -> BuildPlan -> [(PackageName, Version)]
forwardDepsOf name bp = case Map.lookup name (bpUnits bp) of
  Nothing -> []
  Just u  -> [ (pkgName d, pkgVersion d) | d <- puDeps u ]

-- | Get reverse dependencies of a package (packages that depend on it).
reverseDepsOf :: PackageName -> BuildPlan -> [(PackageName, Version)]
reverseDepsOf target bp =
  [ (pkgName (puId u), pkgVersion (puId u))
  | u <- Map.elems (bpUnits bp)
  , any (\d -> pkgName d == target) (puDeps u)
  ]
