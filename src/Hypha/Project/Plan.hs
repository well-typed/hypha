{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Project.Plan
  ( -- * Types
    PlanError (..)
    -- * Loading
  , loadBuildPlan
  ) where

import Control.Exception (IOException, try)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified Cabal.Plan as CP

import Hypha.Types.BuildPlan
  ( BuildPlan (..), CompilerId (..), PlannedUnit (..), ProjectRoot (..) )
import Hypha.Types.PackageId (PackageId (..), PackageName (..), Version (..))

-- | Errors that can occur when loading the build plan.
data PlanError
  = PlanNotFound !FilePath
    -- ^ @plan.json@ does not exist at the expected path.
  | PlanParseFailure !String
    -- ^ The @plan.json@ file could not be decoded.
  deriving stock (Show, Eq)

-- | Load the build plan from @dist-newstyle\/cache\/plan.json@
--   relative to the project root.
--
--   Uses @cabal-plan@'s @findAndDecodePlanJson@ for robust discovery.
loadBuildPlan :: ProjectRoot -> IO (Either PlanError BuildPlan)
loadBuildPlan (ProjectRoot root) = do
  result <- try @IOException (CP.findAndDecodePlanJson (CP.ProjectRelativeToDir root))
  case result of
    Left e  -> pure (Left (PlanNotFound (show e)))
    Right pj -> pure (Right (cabalPlanToBuildPlan pj))

-- | Convert a @cabal-plan@ 'CP.PlanJson' to our simplified 'BuildPlan'.
cabalPlanToBuildPlan :: CP.PlanJson -> BuildPlan
cabalPlanToBuildPlan pj = BuildPlan
  { bpCompiler  = compilerFromPlan pj
  , bpUnits     = unitsFromPlan pj
  , bpOverrides = []
  }

-- | Extract compiler identifier from the plan.
compilerFromPlan :: CP.PlanJson -> CompilerId
compilerFromPlan pj =
  let CP.PkgId (CP.PkgName name) (CP.Ver parts) = CP.pjCompilerId pj
      verStr = Text.intercalate (Text.pack ".") (map (Text.pack . show) parts)
  in CompilerId (name <> Text.pack "-" <> verStr)

-- | Extract planned units with their dependencies from the plan.
unitsFromPlan :: CP.PlanJson -> Map PackageName PlannedUnit
unitsFromPlan pj =
  let allUnits = Map.elems (CP.pjUnits pj)
      -- Build a map from UnitId to PkgId for dependency resolution
      unitIdToPkgId = Map.fromList
        [ (CP.uId u, CP.uPId u)
        | u <- allUnits
        ]
      -- Include all units (local, global, builtin, inplace).
      -- Local packages (hypha itself) are included with puIsLocal = True.
  in Map.fromList
    [ (PackageName pkgText, toPlannedUnit unitIdToPkgId u)
    | u <- allUnits
    , let CP.PkgId (CP.PkgName pkgText) _ = CP.uPId u
    ]

-- | Convert a cabal-plan Unit to our PlannedUnit type.
toPlannedUnit :: Map CP.UnitId CP.PkgId -> CP.Unit -> PlannedUnit
toPlannedUnit unitIdToPkgId u =
  let CP.PkgId (CP.PkgName name) ver = CP.uPId u
      pkgId = PackageId (PackageName name) (Version (CP.dispVer ver))
      -- Get library dependencies from all components
      libDeps = concatMap (Set.toList . CP.ciLibDeps) (Map.elems (CP.uComps u))
      -- Resolve UnitIds to PackageIds
      deps = [ toPackageId pid | uid <- libDeps, Just pid <- [Map.lookup uid unitIdToPkgId] ]
  in PlannedUnit
    { puId      = pkgId
    , puDeps    = deps
    , puIsLocal = (CP.uType u == CP.UnitTypeLocal)
    }

-- | Convert a cabal-plan PkgId to our PackageId type.
toPackageId :: CP.PkgId -> PackageId
toPackageId (CP.PkgId (CP.PkgName name) ver) =
  PackageId (PackageName name) (Version (CP.dispVer ver))
