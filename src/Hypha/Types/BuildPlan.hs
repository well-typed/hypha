{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Types.BuildPlan
  ( -- * Core types
    BuildPlan (..)
  , PlannedUnit (..)
  , PackageOrigin (..)
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
  , topologicalOrder
  , moduleOwner
  ) where

-- Qualified rather than unqualified: base 4.20 added 'foldl'' to the
-- Prelude, so an unqualified import is redundant on GHC 9.10 and required
-- on 9.6, and -Werror rejects whichever one we pick.
import qualified Data.Foldable as Foldable
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)

import Hypha.Project.Components
  ( ComponentInfo (..), ComponentKind )
import Hypha.Types.PackageId (PackageName (..), PackageId (..), Version (..))
import Hypha.Types.SymbolPath (ModulePath (..))

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

-- | A single package entry in the build plan (legacy type; prefer 'PlannedUnit').
data PlanPackage = PlanPackage
  { ppName    :: !PackageName
  , ppVersion :: !Version
  }
  deriving stock (Show, Eq, Ord)

-- | Where the bytes of a planned unit come from.  Derived from
-- @plan.json@'s @pkg-src@ field and the unit type.
--
-- The distinction matters in the @hypha server@ UI: an
-- @acid-state-1.1.1@ pulled from a @source-repository-package@ fork
-- looks identical to the Hackage copy unless we surface the origin.
data PackageOrigin
  = OriginHackage
    -- ^ Pulled from a Hackage-like repo tarball (the common case).
  | OriginSourceRepo
      !(Maybe Text)  -- ^ repo URL, when known
      !(Maybe Text)  -- ^ git tag / branch / commit reference
      !(Maybe FilePath)  -- ^ subdir within the repo
    -- ^ @source-repository-package@ overlay.
  | OriginLocal !FilePath
    -- ^ Local @packages:@ entry; field is the source dir.
  | OriginLocalTarball !FilePath
    -- ^ Locally-stored tarball (uncommon).
  | OriginRemoteTarball !Text
    -- ^ Arbitrary remote tarball URL (uncommon).
  | OriginDistribution
    -- ^ Plan entry without a @pkg-src@ block — typically @base@,
    -- @ghc-prim@ and other libraries shipped with the GHC
    -- distribution.
  deriving stock (Show, Eq, Ord)

-- | A planned unit with its dependencies and metadata.
data PlannedUnit = PlannedUnit
  { puId      :: !PackageId
  , puDeps    :: ![PackageId]
  , puIsLocal :: !Bool
    -- ^ Whether this is a local project package (not a dependency).
  , puOrigin  :: !PackageOrigin
    -- ^ Provenance of the package source (Hackage, SRP overlay, local
    -- directory, etc.), derived from @plan.json@.
  , puSrcDir  :: !(Maybe FilePath)
    -- ^ Source root from plan.json (pkg-src.path).
    -- 'Just p' for inplace/local packages, 'Nothing' otherwise.
  , puDistDir :: !(Maybe FilePath)
    -- ^ Build directory from plan.json (dist-dir).
    -- Used to locate pre-built Haddock HTML for local packages.
  , puLibComponents :: ![ComponentInfo]
    -- ^ Library components (main + sublibs) discovered by parsing
    -- this unit's @.cabal@ file.  An empty list means "no cabal file
    -- found / parse failed" — callers fall back to the heuristic
    -- source-root walk.
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
        PlannedUnit { puId = PackageId n v, puDeps = [], puIsLocal = False
                    , puOrigin = OriginDistribution
                    , puSrcDir = Nothing, puDistDir = Nothing
                    , puLibComponents = [] }

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

-- | The given units, dependencies before dependents.
--
-- Only edges /within/ the input matter: a dependency that is already
-- cached is not in the list and has no order to constrain.  Each distinct
-- unit is emitted exactly once, and nothing is ever dropped — a unit the
-- plan does not know has no edges, and a unit inside a cycle is emitted
-- when its own traversal returns.  So a cycle produces an arbitrary but
-- total order rather than a hang, which matters because a plan is only a
-- DAG by construction, not by type.
--
-- The indexer needs this because a component's cross-package re-exports
-- resolve against the rows its dependencies already produced: @base@ has
-- no signature for @mapAccumL@ until @ghc-internal@ has been indexed.
topologicalOrder :: BuildPlan -> [PackageId] -> [PackageId]
topologicalOrder bp pids = reverse (snd (Foldable.foldl' visit (Set.empty, []) pids))
  where
    wanted = Set.fromList pids

    visit (seen, acc) pid
      | pid `Set.member` seen = (seen, acc)
      | otherwise =
          -- Marked before recursing, so a back edge terminates.  Consing
          -- the unit in front of its own dependencies and reversing at the
          -- end is what puts the dependencies first without an O(n²)
          -- append.
          let (seen', acc') = Foldable.foldl' visit (Set.insert pid seen, acc) (depsOf pid)
          in (seen', pid : acc')

    depsOf pid = case Map.lookup (pkgName pid) (bpUnits bp) of
      Nothing -> []
      Just u  -> [ d | d <- puDeps u, d `Set.member` wanted ]

-- | Which of a unit's dependencies exposes a module, and under which
-- component.
--
-- Answered from the plan alone: no source is read and no index is
-- consulted, so a module page can find the owner of a cross-package
-- re-export before deciding what to load.
--
-- Scoped to the asking unit's dependencies, plus the unit itself so a
-- sub-library re-exporting from the main library resolves.  Two unrelated
-- packages can expose a module of the same name, and only the asking
-- unit's dependency list says which one it meant.  Ties among dependencies
-- break lexicographically, so the answer does not depend on plan order.
--
-- Lives here rather than in "Hypha.Project.Components" — which owns
-- 'ComponentInfo' and would be the natural home — because that module
-- cannot see 'BuildPlan': the dependency runs the other way.
moduleOwner
  :: BuildPlan
  -> PackageId
  -> ModulePath
  -> Maybe (PackageId, ComponentKind)
moduleOwner bp asking m = case sortOn (unPackageName . pkgName . fst) candidates of
  (c : _) -> Just c
  []      -> Nothing
  where
    scope = pkgName asking : case Map.lookup (pkgName asking) (bpUnits bp) of
      Nothing -> []
      Just u  -> map pkgName (puDeps u)

    candidates =
      [ (puId u, ciKind c)
      | n <- scope
      , Just u <- [Map.lookup n (bpUnits bp)]
      , c <- puLibComponents u
      , unModulePath m `elem` ciExposedModules c
      ]
