{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Package resolution with automatic fallback chain.
--
-- When a command needs package metadata or source, the resolver implements
-- the lookup chain from the design spec (Section 7) but without the @--any@
-- flag — widening to Hackage is automatic:
--
--   1. Check the build plan (pinned version from @plan.json@).
--   2. If not in plan, check the cabal store (installed packages).
--   3. If not in store, fetch from Hackage JSON API.
--   4. Cache the result for subsequent lookups.
--
-- Each step tags the result with @outside_plan@ so the envelope can surface it.
module Hypha.Package.Resolver
  ( -- * Types
    ResolvedPackage (..)
  , PackageResolver (..)
    -- * Construction
  , mkPackageResolver
  , mkOfflinePackageResolver
    -- * Operations
  , resolvePackage
  , resolvePackageSource
  , fetchAvailableVersions
  ) where

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
import Hypha.Cache (cacheRoot)
import Hypha.Error (HyphaError (..))
import qualified Hypha.Hackage.Api as Hackage
import Hypha.Hackage.Api (HackageClient (..))
import Hypha.Hackage.Source (fetchAndExtractSource)
import Hypha.Types.BuildPlan (BuildPlan (..), PlannedUnit (..), lookupUnit)
import Hypha.Types.PackageId (PackageId (..), PackageName (..), Version (..))

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
  }
  deriving stock (Show, Eq)

-- | Record-of-functions interface for package resolution.
data PackageResolver m = PackageResolver
  { resolvePkg      :: PackageName -> m (Either HyphaError ResolvedPackage)
  , resolveSrc      :: PackageId -> m (Either HyphaError FilePath)
  , fetchVrs       :: PackageName -> m (Either HyphaError [Version])
  }

-- | Construct a package resolver from its dependencies.
--
-- The fallback chain:
--   1. Build plan (pinned versions from @plan.json@).
--   2. Cabal store (via 'BuildEnv.discoverInstalledPackages').
--   3. Hackage JSON API (latest version from 'fetchPackageJson').
mkPackageResolver
  :: BuildEnv IO
  -> HackageClient IO
  -> BuildPlan
  -> IO (PackageResolver IO)
mkPackageResolver env hclient plan = do
  sourceCache <- (</> "source") <$> cacheRoot
  createDirectoryIfMissing True sourceCache
  pure PackageResolver
    { resolvePkg = resolvePackageWith env hclient plan
    , resolveSrc = resolvePackageSourceWith env hclient sourceCache
    , fetchVrs   = \pkgName -> do
        result <- fetchVersions hclient pkgName
        pure (either (Left . hackageErrorToHypha pkgName) Right result)
    }

-- | Construct a resolver that never hits the network.
--   Only checks the plan and the store; returns 'NotFound' if both miss.
mkOfflinePackageResolver :: BuildEnv IO -> BuildPlan -> PackageResolver IO
mkOfflinePackageResolver env plan = PackageResolver
  { resolvePkg = resolvePackageOffline env plan
  , resolveSrc = \_pid -> pure (Left (EnvError "source lookup unavailable in offline mode"))
  , fetchVrs   = \_pkgName -> pure (Left (NetworkError "version lookup unavailable in offline mode"))
  }

-- | Convenience wrapper: resolve a package using the resolver.
resolvePackage :: PackageResolver IO -> PackageName -> IO (Either HyphaError ResolvedPackage)
resolvePackage = resolvePkg

-- | Convenience wrapper: resolve source for a package using the resolver.
resolvePackageSource :: PackageResolver IO -> PackageId -> IO (Either HyphaError FilePath)
resolvePackageSource = resolveSrc

-- | Convenience wrapper: fetch available versions.
fetchAvailableVersions :: PackageResolver IO -> PackageName -> IO (Either HyphaError [Version])
fetchAvailableVersions = fetchVrs

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
            })
        [] -> do
          -- Step 3: Fetch from Hackage (latest version).
          result <- fetchPackageJson hclient name
          case result of
            Left err -> pure (Left (hackageErrorToHypha name err))
            Right json -> do
              let ver = extractVersion json
                  pid = PackageId name (fromMaybe (Version "unknown") ver)
              pure (Right ResolvedPackage
                { rpPkgId        = pid
                , rpIsOutsidePlan = True
                , rpIsLocal       = False
                , rpDepsCount     = 0
                })
  where
    matchingPid :: PackageName -> PackageId -> Maybe PackageId
    matchingPid target pid
      | pkgName pid == target = Just pid
      | otherwise             = Nothing

-- | Resolve a package name offline (plan + store only).
resolvePackageOffline
  :: BuildEnv IO
  -> BuildPlan
  -> PackageName
  -> IO (Either HyphaError ResolvedPackage)
resolvePackageOffline env plan name = do
  case lookupUnit name plan of
    Just pu ->
      pure (Right ResolvedPackage
        { rpPkgId        = puId pu
        , rpIsOutsidePlan = False
        , rpIsLocal       = puIsLocal pu
        , rpDepsCount     = length (puDeps pu)
        })
    Nothing -> do
      installed <- discoverInstalledPackages env
      case mapMaybe (matchingPid name) (Set.toList installed) of
        (pid : _) ->
          pure (Right ResolvedPackage
            { rpPkgId        = pid
            , rpIsOutsidePlan = True
            , rpIsLocal       = False
            , rpDepsCount     = 0
            })
        [] ->
          pure (Left (NotFound
            ("package '" <> unPackageName name <> "' not in plan or store; try without --offline to reach Hackage")))
  where
    matchingPid :: PackageName -> PackageId -> Maybe PackageId
    matchingPid target pid
      | pkgName pid == target = Just pid
      | otherwise              = Nothing

-- | Resolve the source directory for a package, trying local sources first,
--   then falling back to downloading from Hackage.
resolvePackageSourceWith
  :: BuildEnv IO
  -> HackageClient IO
  -> FilePath     -- ^ source cache directory
  -> PackageId
  -> IO (Either HyphaError FilePath)
resolvePackageSourceWith env hclient sourceCache pid = do
  -- Step 1: Try local source lookup (store, dist-newstyle, project sources).
  mSrc <- locatePackageSource env pid
  case mSrc of
    Just dir -> pure (Right dir)
    Nothing  -> do
      -- Step 2: Download and extract from Hackage.
      let nameStr = Text.unpack (unPackageName (pkgName pid))
          verStr  = Text.unpack (unVersion (pkgVersion pid))
          destDir = sourceCache </> (nameStr <> "-" <> verStr)
      exists <- doesDirectoryExist destDir
      if exists
        then pure (Right destDir)
        else do
          result <- fetchAndExtractSource hclient pid destDir
          case result of
            Left _err ->
              pure (Left (EnvError
                ("source not found for "
                  <> unPackageName (pkgName pid) <> "-" <> unVersion (pkgVersion pid)
                  <> "; try `cabal build` first or check network connectivity")))
            Right path -> pure (Right path)

-- | Extract the version field from a Hackage package JSON response.
extractVersion :: Value -> Maybe Version
extractVersion = \case
  Aeson.Object obj -> do
    Aeson.String ver <- KM.lookup (Key.fromText "version") obj
    Just (Version ver)
  _ -> Nothing

-- | Convert a HackageError to a HyphaError.
hackageErrorToHypha :: PackageName -> Hackage.HackageError -> HyphaError
hackageErrorToHypha name = \case
  Hackage.NetworkError msg -> NetworkError (Text.pack msg)
  Hackage.OfflineCacheMiss _pn -> NotFound
    ("package '" <> unPackageName name <> "' not cached; can't fetch from Hackage in offline mode")
  Hackage.DecodeError msg -> Corruption
    ("Hackage decode error for " <> unPackageName name <> ": " <> Text.pack msg)
  Hackage.HttpError code -> NetworkError
    ("Hackage HTTP " <> Text.pack (show code) <> " for " <> unPackageName name)

-- | 'fromMaybe' replacement (avoids Prelude dependency on 'Maybe').
fromMaybe :: a -> Maybe a -> a
fromMaybe d Nothing  = d
fromMaybe _ (Just x) = x
