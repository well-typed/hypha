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

import Distribution.System (Arch, OS, Platform (..), buildPlatform)
import Distribution.System qualified as System

import Hypha.Cache (sourceCacheRoot)
import qualified Hypha.Hackage.Source as Src
import Hypha.Project.BuildContext (BuildContext (..))
import qualified Hypha.Project.Components as Comp
import qualified Hypha.Project.GhcIncludes as Inc
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
      -- How every module in this plan is to be read: the macros it is
      -- preprocessed against, the compiler headers it may include, and
      -- the platform its stanzas resolve for.  Derived here because this
      -- is the one place that holds the compiler, the target platform and
      -- every package version at once, and derived once rather than per
      -- module.
      ctx   <- buildContextFor cacheRoot pj
      units <- unitsFromPlan pj cache ctx
      pure (Right (BuildPlan
        { bpCompiler     = compilerFromPlan pj
        , bpUnits        = units
        , bpOverrides    = []
        , bpBuildContext = ctx
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

-- | Everything the plan settles about reading its packages' sources.
--
-- Both halves are best-effort and neither is silent: without the macro
-- header every version gate resolves to its oldest branch, and without
-- the compiler's include directory every module that includes
-- @MachDeps.h@ fails to preprocess.  The platform is the plan's own,
-- falling back to this machine's when @plan.json@ names one we do not
-- recognise.
buildContextFor :: FilePath -> CP.PlanJson -> IO BuildContext
buildContextFor cacheRoot pj = do
  mMacroHeader <- macroHeaderFor cacheRoot pj
  includes     <- platformIncludesFor (compilerFromPlan pj)
  pure BuildContext
    { bcCpp = Cpp.CppEnv
        { Cpp.cppPreInclude  = mMacroHeader
        , Cpp.cppIncludeDirs = includes
        }
    , bcPlatform = platformFromPlan pj
    }

-- | The compiler's own header directories, or @[]@ with the reason said
-- out loud.
platformIncludesFor :: CompilerId -> IO [FilePath]
platformIncludesFor cid = do
  r <- Inc.platformIncludeDirs cid
  case r of
    Right dirs -> pure dirs
    Left err   -> do
      hPutStrLn stderr $
        "warning: GHC's own headers unavailable ("
        <> Text.unpack (Inc.renderGhcIncludeError err)
        <> "); modules that #include MachDeps.h or ghcplatform.h \
           \will not preprocess"
      pure []

-- | The platform @plan.json@ was solved for.
--
-- cabal writes @arch@ and @os@ as the strings its own parser reads, so
-- they round-trip; an unrecognised one means a newer cabal than the one
-- we link against, and this machine's platform is a better answer than
-- refusing to resolve any @os()@ condition at all.
platformFromPlan :: CP.PlanJson -> Platform
platformFromPlan pj = Platform arch os
  where
    Platform hostArch hostOs = buildPlatform

    arch :: Arch
    arch = case System.classifyArch System.Permissive (Text.unpack (CP.pjArch pj)) of
      System.OtherArch _ -> hostArch
      a                  -> a

    os :: OS
    os = case System.classifyOS System.Permissive (Text.unpack (CP.pjOs pj)) of
      System.OtherOS _ -> hostOs
      o                -> o

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
  -> BuildContext
     -- ^ How this plan's sources are read: CPP environment + platform.
  -> IO (Map PackageName PlannedUnit)
unitsFromPlan pj sourceCacheLookup ctx = do
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
       pu <- toPlannedUnit unitIdToPkgId sourceCacheLookup ctx u
       let CP.PkgId (CP.PkgName pkgText) _ = CP.uPId u
       pure (PackageName pkgText, pu))
    allUnits
  pure (Map.fromList pairs)

-- | Convert a cabal-plan Unit to our PlannedUnit type.
toPlannedUnit
  :: Map CP.UnitId CP.PkgId
  -> Map FilePath FilePath
  -> BuildContext
  -> CP.Unit
  -> IO PlannedUnit
toPlannedUnit unitIdToPkgId sourceCacheLookup ctx u = do
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
    Just d  -> componentsFor d ctx
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
componentsFor :: FilePath -> BuildContext -> IO [Comp.ComponentInfo]
componentsFor d ctx = do
  mCabal <- Comp.findCabalFile d
  case mCabal of
    Just c  -> Comp.parseLibComponents c d ctx
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
