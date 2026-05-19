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
    , resolveSrc = resolvePackageSourceWith env hclient sourceCache plan
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

-- | Resolve the source directory for a package, trying local sources first,
--   then falling back to downloading from Hackage.
--
-- Resolution order:
--   1. Local package source path from the build plan (puSrcDir, fast).
--   2. Build environment's store lookup (locatePackageSource).
--   3. Hackage source tarball download.
resolvePackageSourceWith
  :: BuildEnv IO
  -> HackageClient IO
  -> FilePath     -- ^ source cache directory
  -> BuildPlan    -- ^ build plan (for local package src dirs)
  -> PackageId
  -> IO (Either HyphaError FilePath)
resolvePackageSourceWith env hclient sourceCache plan pid = do
  -- Step 0: Local package source from plan (fast, no I/O beyond stat).
  case planSrcDir plan (pkgName pid) of
    Just dir -> do
      exists <- doesDirectoryExist dir
      if exists then pure (Right dir) else fallbackToEnv
    Nothing -> fallbackToEnv
  where
    fallbackToEnv = do
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
