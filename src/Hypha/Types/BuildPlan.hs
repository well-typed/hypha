{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Types.BuildPlan
  ( -- * Core types
    BuildPlan (..)
  , PlannedUnit (..)
  , PackageOrigin (..)
  , ProjectRoot (..)
  , CompilerId (..)
  , PackageOverride (..)
    -- * Construction
  , emptyBuildPlan
    -- * Queries
  , lookupPackage
  , lookupUnit
  , lookupUnitId
  , unpinnedUnitId
  , unpinnedUnitIdFor
  , planVersions
  , planUnitIds
  , applyOverrides
  , forwardDepsOf
  , reverseDepsOf
  , topologicalOrder
  ) where

-- Qualified rather than unqualified: base 4.20 added 'foldl'' to the
-- Prelude, so an unqualified import is redundant on GHC 9.10 and required
-- on 9.6, and -Werror rejects whichever one we pick.
import qualified Data.Foldable as Foldable
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)

import Hypha.Project.BuildContext (BuildContext, hostBuildContext)
import Hypha.Project.Components (ComponentInfo)
import Hypha.Types.PackageId
  ( PackageId (..), PackageName (..), UnitId (..), Version (..) )

-- | Absolute path to the project root (directory containing @cabal.project@).
newtype ProjectRoot = ProjectRoot FilePath
  deriving stock   (Show, Eq, Ord)
  deriving newtype (Read)

-- | GHC compiler identifier, e.g. @ghc-9.6.7@.
newtype CompilerId = CompilerId { unCompilerId :: Text }
  deriving stock   (Show, Eq, Ord)
  deriving newtype (Read)

-- | A user-supplied version override, e.g. @async=2.2.6@.
data PackageOverride = PackageOverride
  { poName    :: !PackageName
  , poVersion :: !Version
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
  , puUnitId  :: !UnitId
    -- ^ cabal's id for this unit's configuration, straight from
    -- @plan.json@.  What the index cache keys on, so two projects that
    -- resolved a package differently do not share (or evict) each
    -- other's rows.
  , puDeps    :: ![PackageId]
  , puIsLocal :: !Bool
    -- ^ Whether this is a local project package (not a dependency).
  , puOrigin  :: !PackageOrigin
    -- ^ Provenance of the package source (Hackage, SRP overlay, local
    -- directory, etc.), derived from @plan.json@.
  , puSrcDir  :: !(Maybe FilePath)
    -- ^ Where the unit's source is on disk: @pkg-src.path@ for a local
    -- package, cabal's @dist-newstyle/src@ checkout for a
    -- @source-repository-package@, 'Nothing' for a store package.
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
  , bpBuildContext :: !BuildContext
    -- ^ How this plan's sources are to be read: the CPP environment
    -- (synthesised @cabal_macros.h@ and the compiler's own include
    -- directory) and the platform its conditional stanzas resolve for.
    -- Carried here because every consumer that needs to preprocess a
    -- module already holds the plan, and it must describe the same
    -- compiler, versions and platform the rest of the answers do.
  }
  deriving stock (Show)

-- | An empty build plan (no packages, no compiler).
emptyBuildPlan :: BuildPlan
emptyBuildPlan = BuildPlan
  { bpCompiler  = CompilerId "unknown"
  , bpUnits     = Map.empty
  , bpOverrides = []
  , bpBuildContext = hostBuildContext
  }

-- | Look up a package version in the plan.
lookupPackage :: PackageName -> BuildPlan -> Maybe Version
lookupPackage name bp = pkgVersion . puId <$> Map.lookup name (bpUnits bp)

-- | The configuration the plan resolved for a package, when it has one.
--
-- Read rather than reconstructed: every caller that keys cache rows needs
-- the same answer, and a second way to derive it would be a second answer.
lookupUnitId :: PackageName -> BuildPlan -> Maybe UnitId
lookupUnitId name bp = puUnitId <$> Map.lookup name (bpUnits bp)

-- | The unit-id the plan pins for each package it mentions.
planUnitIds :: BuildPlan -> Map PackageName UnitId
planUnitIds = Map.map puUnitId . bpUnits

-- | The version this plan pins for each package it mentions.
--
-- The whole plan reduced to what a version check needs, so callers that
-- only want "is this the version we build against" do not carry a
-- 'BuildPlan' (and its source directories, dep lists and component
-- inventories) around to ask it.
planVersions :: BuildPlan -> Map PackageName Version
planVersions = Map.map (pkgVersion . puId) . bpUnits

-- | Look up a planned unit in the plan.
lookupUnit :: PackageName -> BuildPlan -> Maybe PlannedUnit
lookupUnit name = Map.lookup name . bpUnits

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
        PlannedUnit { puId = PackageId n v
                    , puUnitId = unpinnedUnitId n v
                    , puDeps = [], puIsLocal = False
                    , puOrigin = OriginDistribution
                    , puSrcDir = Nothing, puDistDir = Nothing
                    , puLibComponents = [] }

-- | The pseudo-configuration a package no plan resolved is indexed under.
--
-- Two ways to get here: @--package-override PKG=VER@ names a version the
-- plan does not build, and a store-only unit (no project, so no plan at
-- all) was never resolved by cabal either.  Neither has a unit-id to
-- borrow, and a distinct, self-describing one beats the alternatives:
-- reusing some real configuration's id would let these rows answer a
-- plan-scoped lookup, and an empty id would make "no configuration" a
-- value every reader has to remember to reject.  Reads for such a package
-- match on version instead ('Hypha.Search.Cache.PinVersion'), so the rows
-- stay findable by the run that asked for them.
unpinnedUnitId :: PackageName -> Version -> UnitId
unpinnedUnitId (PackageName n) (Version v) =
  UnitId ("unpinned:" <> n <> "-" <> v)

-- | 'unpinnedUnitId' for a whole 'PackageId'.
unpinnedUnitIdFor :: PackageId -> UnitId
unpinnedUnitIdFor pid = unpinnedUnitId (pkgName pid) (pkgVersion pid)

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
