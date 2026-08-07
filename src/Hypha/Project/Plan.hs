{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Project.Plan
  ( -- * Types
    PlanError (..)
    -- * Loading
  , loadBuildPlan
  , loadPlanVersions
    -- * Staleness
  , planHash
  ) where

import Control.Exception.Safe (IOException, try)
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString.Base16 as Base16
import Data.List (sort, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import qualified Cabal.Plan as CP

import Hypha.Cache (sourceCacheRoot)
import qualified Hypha.Hackage.Source as Src
import qualified Hypha.Project.Components as Comp
import qualified Hypha.Source.CppMacros as Cpp
import System.IO (hPutStrLn, stderr)
import Hypha.Types.BuildPlan
  ( BuildPlan (..), CompilerId (..), PackageOrigin (..), PlannedUnit (..)
  , ProjectRoot (..) )
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
loadBuildPlan :: FilePath -> ProjectRoot -> IO (Either PlanError BuildPlan)
loadBuildPlan cacheRoot (ProjectRoot root) = do
  result <- try @IO @IOException
              (CP.findAndDecodePlanJson (CP.ProjectRelativeToDir root))
  case result of
    Left e   -> pure (Left (PlanNotFound (show e)))
    Right pj -> do
      cache <- Src.enumerateSourceCache (sourceCacheRoot cacheRoot)
      -- The macro environment every module in this plan is preprocessed
      -- against.  Derived here because this is the one place that holds
      -- the compiler and every package version at once, and materialised
      -- once rather than per module.
      mMacroHeader <- macroHeaderFor cacheRoot pj
      units <- unitsFromPlan pj cache mMacroHeader
      pure (Right (BuildPlan
        { bpCompiler  = compilerFromPlan pj
        , bpUnits     = units
        , bpOverrides = []
        , bpCppMacros = mMacroHeader
        }))

-- | Just the versions the plan pins, one per package.
--
-- 'loadBuildPlan' resolves a source directory per unit and reads a
-- @.cabal@ file for each one to inventory its components — hundreds of
-- file reads and cabal parses on a real plan.  A caller that only wants
-- to know which versions this project builds against needs none of it,
-- and @hypha lookup@ is on the tier-1 fast path where that work is the
-- dominant cost.  Kept beside 'loadBuildPlan' so the two read the same
-- @plan.json@ through the same discovery, and pinned to agreement by a
-- test rather than by comment.
loadPlanVersions :: ProjectRoot -> IO (Either PlanError (Map PackageName Version))
loadPlanVersions (ProjectRoot root) = do
  result <- try @IO @IOException
              (CP.findAndDecodePlanJson (CP.ProjectRelativeToDir root))
  pure $ case result of
    Left e   -> Left (PlanNotFound (show e))
    Right pj -> Right (Map.fromList
      [ (PackageName name, Version (CP.dispVer ver))
      | u <- Map.elems (CP.pjUnits pj)
      , let CP.PkgId (CP.PkgName name) ver = CP.uPId u
      ])

-- | Write the @cabal_macros.h@ this plan implies, and return its path.
--
-- Best-effort: a cache root that cannot be written is not a reason to
-- fail loading a plan, but it is a reason to say so — without the header
-- every version gate silently resolves to its oldest branch.
macroHeaderFor :: FilePath -> CP.PlanJson -> IO (Maybe FilePath)
macroHeaderFor cacheRoot pj = do
  let CompilerId compiler = compilerFromPlan pj
      pkgs =
        [ (PackageName name, Version (CP.dispVer ver))
        | u <- Map.elems (CP.pjUnits pj)
        , let CP.PkgId (CP.PkgName name) ver = CP.uPId u
        ]
      header = Cpp.renderMacroHeader compiler pkgs
  r <- try @IO @IOException (Cpp.materialiseMacroHeader cacheRoot header)
  case r of
    Right path -> pure (Just path)
    Left err   -> do
      hPutStrLn stderr $
        "warning: CPP macros unavailable (" <> show err
        <> "); modules guarded by #if __GLASGOW_HASKELL__ or MIN_VERSION_* \
           \will be read from their oldest branch"
      pure Nothing

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
  -> Maybe FilePath
     -- ^ Synthesised @cabal_macros.h@ for this plan, when one could be
     -- written.
  -> IO (Map PackageName PlannedUnit)
unitsFromPlan pj sourceCacheLookup mMacroHeader = do
      -- A package contributes one unit per component (lib, exes, test
      -- suites).  The map below is keyed by package *name* and
      -- 'Map.fromList' retains the last duplicate, so order units
      -- with the library-carrying one last: it is the unit whose
      -- dist-dir holds the rendered Haddock and whose dependencies
      -- describe the library.  Without this, hypha's own test-suite
      -- unit used to win and @puDistDir@ pointed at @t/<pkg>-tests@.
  let allUnits = sortOn (\u -> CP.CompNameLib `Map.member` CP.uComps u)
                        (Map.elems (CP.pjUnits pj))
      unitIdToPkgId = Map.fromList
        [ (CP.uId u, CP.uPId u) | u <- allUnits ]
  pairs <- mapM
    (\u -> do
       pu <- toPlannedUnit unitIdToPkgId sourceCacheLookup mMacroHeader u
       let CP.PkgId (CP.PkgName pkgText) _ = CP.uPId u
       pure (PackageName pkgText, pu))
    allUnits
  pure (Map.fromList pairs)

-- | Convert a cabal-plan Unit to our PlannedUnit type.
toPlannedUnit
  :: Map CP.UnitId CP.PkgId
  -> Map FilePath FilePath
  -> Maybe FilePath
  -> CP.Unit
  -> IO PlannedUnit
toPlannedUnit unitIdToPkgId sourceCacheLookup mMacroHeader u = do
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
    Just d  -> componentsFor d mMacroHeader
    Nothing -> pure []
  pure PlannedUnit
    { puId            = pkgId
    , puDeps          = deps
    , puIsLocal       = (CP.uType u == CP.UnitTypeLocal)
    , puOrigin        = originFromPkgLoc (CP.uPkgSrc u)
    , puSrcDir        = srcDir
    , puDistDir       = CP.uDistDir u
    , puLibComponents = comps
    }

-- | Parse the @.cabal@ file in a directory and return its library
-- components.  Silent fallback to @[]@ on any kind of failure.
componentsFor :: FilePath -> Maybe FilePath -> IO [Comp.ComponentInfo]
componentsFor d mMacroHeader = do
  mCabal <- Comp.findCabalFile d
  case mCabal of
    Just c  -> Comp.parseLibComponents c d mMacroHeader
    Nothing -> pure []

-- | Extract the source directory from a @PkgLoc@ value.
-- Returns 'Just p' for 'LocalUnpackedPackage' (inplace/local packages),
-- 'Nothing' for all other package source types.
extractSrcDir :: Maybe CP.PkgLoc -> Maybe FilePath
extractSrcDir (Just (CP.LocalUnpackedPackage p)) = Just p
extractSrcDir _                                   = Nothing

-- | Map a @cabal-plan@ source location to our coarser 'PackageOrigin'.
-- We do not surface 'OriginSourceRepo' metadata that the plan omits;
-- if cabal didn't record a URL, neither do we.
originFromPkgLoc :: Maybe CP.PkgLoc -> PackageOrigin
originFromPkgLoc = \case
  Nothing -> OriginDistribution
  Just (CP.LocalUnpackedPackage p) -> OriginLocal p
  Just (CP.LocalTarballPackage  p) -> OriginLocalTarball p
  Just (CP.RemoteTarballPackage (CP.URI u)) -> OriginRemoteTarball u
  Just (CP.RepoTarballPackage _)   -> OriginHackage
  Just (CP.RemoteSourceRepoPackage sr) ->
    OriginSourceRepo
      (CP.srLocation sr)
      -- Prefer explicit tag, fall back to branch — cabal stores the
      -- resolved commit hash in @tag@ for @source-repository-package@
      -- pinned via @tag:@ but in @branch@ when only a branch is given.
      (firstJust (CP.srTag sr) (CP.srBranch sr))
      (CP.srSubdir sr)

-- | Convert a cabal-plan PkgId to our PackageId type.
toPackageId :: CP.PkgId -> PackageId
toPackageId (CP.PkgId (CP.PkgName name) ver) =
  PackageId (PackageName name) (Version (CP.dispVer ver))

-- | Like @<|>@ on 'Maybe', spelt out to keep the dependency surface
-- small; @Control.Applicative@ would do but we already avoid importing
-- it here.
firstJust :: Maybe a -> Maybe a -> Maybe a
firstJust (Just x) _ = Just x
firstJust Nothing  y = y

-- | SHA-256 (hex) over the in-memory plan.  Used as the staleness
-- stamp for the local Hoogle DB: when the set of pinned
-- @(pkg, version)@ pairs changes, the hash changes too.  We hash the
-- canonical form rather than the on-disk @plan.json@ so that
-- semantically identical plans produce identical hashes.
planHash :: BuildPlan -> Text
planHash bp =
  let pids = sort
        [ unPackageName (pkgName (puId u))
          <> "-"
          <> unVersion (pkgVersion (puId u))
        | u <- Map.elems (bpUnits bp)
        ]
      payload = Text.encodeUtf8 (Text.unlines pids)
      digest  = SHA256.hash payload
  in Text.decodeUtf8 (Base16.encode digest)
