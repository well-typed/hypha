{-# LANGUAGE DerivingStrategies #-}
module Hypha.Project.Plan
  ( -- * Types
    PlanError (..)
    -- * Loading
  , loadBuildPlan
  ) where

import Control.Exception (IOException, try)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified Cabal.Plan as CP

import Hypha.Types.BuildPlan
  ( BuildPlan (..), PlanPackage (..), CompilerId (..), ProjectRoot (..) )
import Hypha.Types.PackageId (PackageName (..), Version (..))

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
  , bpPackages  = packagesFromPlan pj
  , bpOverrides = []
  }

-- | Extract compiler identifier from the plan.
compilerFromPlan :: CP.PlanJson -> CompilerId
compilerFromPlan pj =
  let CP.PkgId (CP.PkgName name) (CP.Ver parts) = CP.pjCompilerId pj
      verStr = Text.intercalate (Text.pack ".") (map (Text.pack . show) parts)
  in CompilerId (name <> Text.pack "-" <> verStr)

-- | Extract all packages from the build plan with metadata (local flag,
--   library dependency count).  Deduplicates by package name when multiple
--   units share the same package (e.g. library + executable).
packagesFromPlan :: CP.PlanJson -> Map.Map PackageName PlanPackage
packagesFromPlan pj =
  Map.fromListWith mergePlanPackage
    [ (PackageName pkgText, PlanPackage
        { ppVersion  = Version (CP.dispVer ver)
        , ppIsLocal  = utype == CP.UnitTypeLocal
        , ppDeps     = libDepCount comps
        })
    | CP.Unit { CP.uPId  = CP.PkgId (CP.PkgName pkgText) ver
              , CP.uType  = utype
              , CP.uComps = comps
              } <- Map.elems (CP.pjUnits pj)
    ]
  where
    -- | Count library-level dependencies from a component map.
    libDepCount :: Map.Map CP.CompName CP.CompInfo -> Int
    libDepCount comps = case Map.lookup CP.CompNameLib comps of
      Just (CP.CompInfo { CP.ciLibDeps = deps }) -> Set.size deps
      Nothing                                     -> 0

    -- | Merge two entries for the same package name:
    --   - local wins if either component is local
    --   - take the larger deps count
    mergePlanPackage :: PlanPackage -> PlanPackage -> PlanPackage
    mergePlanPackage a b = PlanPackage
      { ppVersion  = ppVersion a
      , ppIsLocal  = ppIsLocal a || ppIsLocal b
      , ppDeps     = max (ppDeps a) (ppDeps b)
      }
