{-# LANGUAGE DerivingStrategies #-}
module Hypha.Project.Plan
  ( -- * Types
    PlanError (..)
    -- * Loading
  , loadBuildPlan
  ) where

import Control.Exception (IOException, try)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

import qualified Cabal.Plan as CP

import Hypha.Types.BuildPlan
  ( BuildPlan (..), CompilerId (..), ProjectRoot (..) )
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

-- | Extract the set of non-builtin, non-local packages with their versions.
packagesFromPlan :: CP.PlanJson -> Map.Map PackageName Version
packagesFromPlan pj =
  Map.fromList
    [ (PackageName pkgText, Version (CP.dispVer ver))
    | CP.Unit { CP.uPId = CP.PkgId (CP.PkgName pkgText) ver, CP.uType = utype } <- Map.elems (CP.pjUnits pj)
    , utype /= CP.UnitTypeLocal
    ]
