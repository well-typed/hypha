{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Package resolution with automatic fallback chain.
--
-- When a command needs package metadata or source, the resolver implements
-- the seamless lookup chain from the design spec (Section 7).  No @--any@
-- flag — widening is automatic:
--
--   1. Local HTTP cache (XDG cache, consulted by 'HackageClient').
--   2. Build plan (pinned version from @plan.json@).
--   3. Cabal store (installed packages).
--   4. Hackage JSON API (latest version).
--
-- Each step tags the result with @outside_plan@ so the envelope can surface it.
module Hypha.Package.Resolver
  ( -- * Types
    ResolvedPackage (..)
  , PackageResolver (..)
    -- * Construction
  , mkPackageResolver
    -- * Queries
  , planSrcDir
  , resolveRef
  ) where

import Control.Monad (msum)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.Aeson (Value)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import qualified Data.Text as Text
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import System.FilePath ((</>))

import Hypha.BuildEnv.Type (BuildEnv (..))
import qualified Hypha.Cabal.RepoCache as RepoCache
import Hypha.Cache (sourceCacheRoot)
import Hypha.Error (HyphaError (..), NotFoundReason (..))
import qualified Hypha.Hackage.Api as Hackage
import Hypha.Hackage.Api (HackageClient (..), HackageError (..))
import Hypha.Hackage.Source (fetchAndExtractSource)
import Hypha.Types.BuildPlan
  ( BuildPlan (..), PackageOrigin (..), PlannedUnit (..), lookupUnit )
import Hypha.Types.PackageId
  ( PackageId (..), PackageName (..), PackageRef (..), Version (..)
  , renderPackageId )

-- | The result of resolving a package name.
data ResolvedPackage = ResolvedPackage
  { rpPkgId          :: !PackageId
    -- ^ Resolved package identifier (name + version).
  , rpIsOutsidePlan   :: !Bool
    -- ^ Whether this result came from outside the build plan.
  , rpIsLocal         :: !Bool
    -- ^ Whether this is a local project package.
  , rpDepsCount       :: !Int
    -- ^ Number of library-level dependencies (from plan or store).
  , rpOrigin          :: !PackageOrigin
    -- ^ Provenance of the package source.  Out-of-plan resolutions
    -- (store hit, Hackage fallback) report 'OriginHackage'.
  }
  deriving stock (Show, Eq)

-- | Record-of-functions interface for package resolution.  The
-- @resolvePkg@ field still takes a bare 'PackageName' (no version
-- pin) for callers that only know a name; 'resolveRef' wraps it with
-- 'PackageRef' support so a user-supplied @pkg-version@ string is
-- honoured end-to-end.
data PackageResolver m = PackageResolver
  { resolvePkg      :: PackageName -> m (Either HyphaError ResolvedPackage)
  , resolveSrc      :: PackageId -> m (Either HyphaError FilePath)
  , resolveSrcLocal :: PackageId -> m (Maybe FilePath)
    -- ^ 'resolveSrc' without the last two steps: the plan's source path,
    -- the build environment, and hypha's own extracted-source cache, all
    -- of which are directory probes.  No tarball extraction, no HTTP.
    --
    -- Named separately rather than folded into 'resolveSrc' because the
    -- two answer different questions.  A caller that has been /asked/ for
    -- a package should pay a download to answer; a caller probing
    -- speculatively — \"does any of these forty dependencies happen to
    -- declare the module this re-export named?\" — must not, and the cost
    -- of getting that wrong is a download per probe.  'Maybe' rather than
    -- 'Either' for the same reason: not having a package unpacked is the
    -- expected answer here, not a failure.
  , fetchVrs       :: PackageName -> m (Either HyphaError [Version])
  }

-- | Resolve a 'PackageRef'.  When the ref carries a version hint, the
-- plan-and-store lookup is filtered to that exact version and a final
-- Hackage fallback constructs a 'PackageId' from the hint directly
-- (no JSON round-trip, no @\"unknown\"@ defaulting).  When the hint
-- is absent, delegates to 'resolvePkg'.
resolveRef
  :: Monad m => PackageResolver m -> PackageRef -> m (Either HyphaError ResolvedPackage)
resolveRef pr (PackageRef name Nothing)  = resolvePkg pr name
resolveRef _  (PackageRef name (Just v)) = pure $ Right ResolvedPackage
  { rpPkgId         = PackageId name v
  , rpIsOutsidePlan = True
  , rpIsLocal       = False
  , rpDepsCount     = 0
  , rpOrigin        = OriginHackage
  }

-- Pinned versions bypass plan / store and go straight to Hackage via
-- 'resolveSrc'.  Existence is validated when the source is fetched;
-- a missing version surfaces as 'NotFound' from the tarball
-- downloader rather than being silently invented here.

-- | Construct a package resolver from its dependencies.
--
-- The fallback chain:
--   1. Build plan (pinned versions from @plan.json@).
--   2. Cabal store (via 'BuildEnv.discoverInstalledPackages').
--   3. Hackage JSON API (latest version from 'fetchPackageJson').
mkPackageResolver
  :: BuildEnv IO
  -> HackageClient IO
  -> FilePath     -- ^ cache root (for source cache)
  -> BuildPlan
  -> IO (PackageResolver IO)
mkPackageResolver env hclient cacheRoot plan = do
  let sourceCache = sourceCacheRoot cacheRoot
  createDirectoryIfMissing True sourceCache
  pure PackageResolver
    { resolvePkg      = resolvePackageWith env hclient plan
    , resolveSrc      = resolvePackageSourceWith env hclient sourceCache plan
    , resolveSrcLocal = localPackageSource env sourceCache plan
    , fetchVrs   = \pkgName -> do
        result <- fetchVersions hclient pkgName
        pure (either (Left . hackageErrorToHypha pkgName) Right result)
    }

-- | Resolve a package name against the full fallback chain.
resolvePackageWith
  :: BuildEnv IO
  -> HackageClient IO
  -> BuildPlan
  -> PackageName
  -> IO (Either HyphaError ResolvedPackage)
resolvePackageWith env hclient plan name = do
  -- Step 1: Try the build plan (pinned version).
  case lookupUnit name plan of
    Just pu ->
      pure (Right ResolvedPackage
        { rpPkgId        = puId pu
        , rpIsOutsidePlan = False
        , rpIsLocal       = puIsLocal pu
        , rpDepsCount     = length (puDeps pu)
        , rpOrigin        = puOrigin pu
        })
    Nothing -> do
      -- Step 2: Try the cabal store (installed packages).
      installed <- discoverInstalledPackages env
      case mapMaybe (matchingPid name) (Set.toList installed) of
        (pid : _) ->
          pure (Right ResolvedPackage
            { rpPkgId        = pid
            , rpIsOutsidePlan = True
            , rpIsLocal       = False
            , rpDepsCount     = 0
            , rpOrigin        = OriginHackage
            })
        [] -> do
          -- Step 3: Fetch from Hackage (latest version).
          result <- fetchPackageJson hclient name
          case result of
            Left err   -> pure (Left (hackageErrorToHypha name err))
            Right json -> case extractVersion json of
              Just ver -> pure (Right ResolvedPackage
                { rpPkgId         = PackageId name ver
                , rpIsOutsidePlan = True
                , rpIsLocal       = False
                , rpDepsCount     = 0
                , rpOrigin        = OriginHackage
                })
              Nothing  -> pure (Left
                (HackageFailure name (Hackage.MissingField "version")))
  where
    matchingPid :: PackageName -> PackageId -> Maybe PackageId
    matchingPid target pid
      | pkgName pid == target = Just pid
      | otherwise             = Nothing

-- | Resolve the source directory for a package, trying local sources
-- first and falling back to a network fetch only as a last resort.
--
-- Resolution order:
--
--   0. Local package source path from the build plan (@puSrcDir@, fast).
--   1. Build environment's source lookup ('locatePackageSource' — store,
--      dist-newstyle, project sources).
--   2. Hypha's own extracted-source cache at
--      @$XDG_CACHE_HOME/hypha/source/pkg-ver/@ (populated by a previous
--      step 3 or 4 invocation).
--   3. Build environment's repo-tarball lookup ('locateRepoTarball' —
--      cabal-install's @~/.cabal/packages/<repo>/.../<pkg>-<ver>.tar.gz@).
--      The tarball is extracted into the step-2 cache so subsequent
--      calls short-circuit there.
--   4. HTTP GET against Hackage.
resolvePackageSourceWith
  :: BuildEnv IO
  -> HackageClient IO
  -> FilePath     -- ^ source cache directory
  -> BuildPlan    -- ^ build plan (for local package src dirs)
  -> PackageId
  -> IO (Either HyphaError FilePath)
resolvePackageSourceWith env hclient sourceCache plan pid =
  -- Steps 0-2 are no-cost cache probes that return 'Just' on a hit.
  -- Steps 3-4 can fail with a structured 'HyphaError', so they run
  -- outside 'MaybeT' over plain 'Either'.
  localPackageSource env sourceCache plan pid
    >>= maybe materialise (pure . Right)
  where
    destDir = extractedSourceDir sourceCache pid

    -- | None of the cache layers had it; produce a directory by
    -- extracting from the cabal repo tarball or, failing that, by
    -- downloading.  Preserves the structured 'HackageError' /
    -- 'TarballFailure' cause through 'hackageErrorToHypha' rather
    -- than collapsing variants.
    materialise :: IO (Either HyphaError FilePath)
    materialise = do
      mTar <- locateRepoTarball env pid
      case mTar of
        Just tarball -> do
          r <- RepoCache.extractTarballGz tarball destDir
          pure $ case r of
            Right ()  -> Right destDir
            Left tErr -> Left (hackageErrorToHypha (pkgName pid) (TarballFailure tErr))
        Nothing -> do
          r <- fetchAndExtractSource hclient pid destDir
          pure $ case r of
            Right path -> Right path
            Left hErr  -> Left (hackageErrorToHypha (pkgName pid) hErr)

-- | Steps 0-2 of 'resolvePackageSourceWith': the plan's own source path
-- for a local package, the build environment (store, dist-newstyle,
-- project sources), and hypha's extracted-source cache.  Every step is a
-- directory probe, so this is the resolution a caller can afford to run
-- speculatively.
--
-- The 'Alternative' instance for 'MaybeT' linearises the chain so the
-- first hit wins without nested case-of (CLAUDE.md "mtl over zig-zags").
localPackageSource
  :: BuildEnv IO
  -> FilePath     -- ^ source cache directory
  -> BuildPlan
  -> PackageId
  -> IO (Maybe FilePath)
localPackageSource env sourceCache plan pid = runMaybeT $ msum
  [ MaybeT (existingDir (planSrcDir plan (pkgName pid)))
  , MaybeT (locatePackageSource env pid)
  , MaybeT (existingDir (Just (extractedSourceDir sourceCache pid)))
  ]
  where
    -- Pass through a 'Just dir' iff @dir@ exists on disk; otherwise
    -- 'Nothing'.  Lifts 'planSrcDir' / cached-extract paths into the
    -- 'MaybeT' chain without a separate case-of per step.
    existingDir Nothing    = pure Nothing
    existingDir (Just dir) = do
      ok <- doesDirectoryExist dir
      pure (if ok then Just dir else Nothing)

-- | Where a fetched tarball is extracted to, and therefore where a
-- previous fetch left it.  One derivation, so the probe and the extract
-- cannot disagree about the path.
extractedSourceDir :: FilePath -> PackageId -> FilePath
extractedSourceDir sourceCache pid =
  sourceCache </> Text.unpack (renderPackageId pid)

-- | Look up the package source directory from the build plan's 'puSrcDir'.
-- Returns 'Just dir' only for local (inplace) packages that have a
-- 'LocalUnpackedPackage' entry in the plan.  Returns 'Nothing' for
-- store/Hackage packages.
planSrcDir :: BuildPlan -> PackageName -> Maybe FilePath
planSrcDir plan name =
  case lookupUnit name plan of
    Just pu | Just dir <- puSrcDir pu -> Just dir
    _                                 -> Nothing

-- | Extract the version field from a Hackage package JSON response.
extractVersion :: Value -> Maybe Version
extractVersion = \case
  Aeson.Object obj -> do
    Aeson.String ver <- KM.lookup (Key.fromText "version") obj
    Just (Version ver)
  _ -> Nothing

-- | Convert a 'Hackage.HackageError' to a 'HyphaError'.  Offline-cache
-- miss maps to the conceptually-correct 'NotFound' umbrella; every
-- other variant is preserved structurally under 'HackageFailure', so
-- the wire layer can dispatch on the variant for the right wire code
-- / exit code (transport ↔ NETWORK_ERROR, decode/missing-field ↔
-- CORRUPTION) and the user sees the right message.
hackageErrorToHypha :: PackageName -> Hackage.HackageError -> HyphaError
hackageErrorToHypha name = \case
  Hackage.OfflineCacheMiss _ -> NotFound (NotFoundOfflineCache name)
  err                        -> HackageFailure name err

