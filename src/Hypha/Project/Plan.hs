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

import qualified Hypha.Hackage.Source as Src
import qualified Hypha.Project.Components as Comp
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
--   For every unit whose source directory is known (either inplace via
--   @pkg-src.path@, or — see 'Hypha.Project.Plan' Task 5 — resolved
--   through the source cache) we read its @.cabal@ file and stash the
--   list of library components on the 'PlannedUnit'.  This drives
--   sub-library indexing in @hypha server@.
loadBuildPlan :: ProjectRoot -> IO (Either PlanError BuildPlan)
loadBuildPlan (ProjectRoot root) = do
  result <- try @IOException
              (CP.findAndDecodePlanJson (CP.ProjectRelativeToDir root))
  case result of
    Left e   -> pure (Left (PlanNotFound (show e)))
    Right pj -> do
      cache <- Src.enumerateSourceCache
      units <- unitsFromPlan pj cache
      pure (Right (BuildPlan
        { bpCompiler  = compilerFromPlan pj
        , bpUnits     = units
        , bpOverrides = []
        }))

-- | Extract compiler identifier from the plan.
compilerFromPlan :: CP.PlanJson -> CompilerId
compilerFromPlan pj =
  let CP.PkgId (CP.PkgName name) (CP.Ver parts) = CP.pjCompilerId pj
      verStr = Text.intercalate (Text.pack ".") (map (Text.pack . show) parts)
  in CompilerId (name <> Text.pack "-" <> verStr)

-- | Extract planned units with their dependencies from the plan.
unitsFromPlan
  :: CP.PlanJson
  -> Map FilePath FilePath
     -- ^ @\"pkg-ver\" -> sourceDir@ for dependency packages.  Empty in
     -- this task; Task 5 populates it from the source cache.
  -> IO (Map PackageName PlannedUnit)
unitsFromPlan pj sourceCacheLookup = do
  let allUnits     = Map.elems (CP.pjUnits pj)
      unitIdToPkgId = Map.fromList
        [ (CP.uId u, CP.uPId u) | u <- allUnits ]
  pairs <- mapM
    (\u -> do
       pu <- toPlannedUnit unitIdToPkgId sourceCacheLookup u
       let CP.PkgId (CP.PkgName pkgText) _ = CP.uPId u
       pure (PackageName pkgText, pu))
    allUnits
  pure (Map.fromList pairs)

-- | Convert a cabal-plan Unit to our PlannedUnit type.
toPlannedUnit
  :: Map CP.UnitId CP.PkgId
  -> Map FilePath FilePath
  -> CP.Unit
  -> IO PlannedUnit
toPlannedUnit unitIdToPkgId sourceCacheLookup u = do
  let CP.PkgId (CP.PkgName name) ver = CP.uPId u
      pkgId   = PackageId (PackageName name) (Version (CP.dispVer ver))
      libDeps = concatMap (Set.toList . CP.ciLibDeps) (Map.elems (CP.uComps u))
      deps    = [ toPackageId pid
                | uid <- libDeps
                , Just pid <- [Map.lookup uid unitIdToPkgId]
                ]
      srcDir  = extractSrcDir (CP.uPkgSrc u)
      depKey  = Text.unpack name <> "-" <> Text.unpack (CP.dispVer ver)
      sourceDir = case srcDir of
        Just d  -> Just d
        Nothing -> Map.lookup depKey sourceCacheLookup
  comps <- case sourceDir of
    Just d  -> componentsFor d
    Nothing -> pure []
  pure PlannedUnit
    { puId            = pkgId
    , puDeps          = deps
    , puIsLocal       = (CP.uType u == CP.UnitTypeLocal)
    , puSrcDir        = srcDir
    , puDistDir       = CP.uDistDir u
    , puLibComponents = comps
    }

-- | Parse the @.cabal@ file in a directory and return its library
-- components.  Silent fallback to @[]@ on any kind of failure.
componentsFor :: FilePath -> IO [Comp.ComponentInfo]
componentsFor d = do
  mCabal <- Comp.findCabalFile d
  case mCabal of
    Just c  -> Comp.parseLibComponents c d
    Nothing -> pure []

-- | Extract the source directory from a @PkgLoc@ value.
-- Returns 'Just p' for 'LocalUnpackedPackage' (inplace/local packages),
-- 'Nothing' for all other package source types.
extractSrcDir :: Maybe CP.PkgLoc -> Maybe FilePath
extractSrcDir (Just (CP.LocalUnpackedPackage p)) = Just p
extractSrcDir _                                   = Nothing

-- | Convert a cabal-plan PkgId to our PackageId type.
toPackageId :: CP.PkgId -> PackageId
toPackageId (CP.PkgId (CP.PkgName name) ver) =
  PackageId (PackageName name) (Version (CP.dispVer ver))
