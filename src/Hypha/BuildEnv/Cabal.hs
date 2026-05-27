{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
module Hypha.BuildEnv.Cabal
  ( -- * Types
    CabalStoreError (..)
    -- * Construction
  , mkCabalBuildEnv
  ) where

import Control.Exception.Safe (IOException, try)
import Data.List (find, isPrefixOf)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import System.Directory
  ( doesDirectoryExist, doesFileExist, getHomeDirectory, listDirectory )
import System.Environment (lookupEnv)
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.IO (stderr)

import Hypha.BuildEnv.Type (BuildEnv (..))
import qualified Hypha.Cabal.RepoCache as RepoCache
import Hypha.Cabal.RepoCache (renderRepoLookupError)
import Hypha.Types.PackageId
  ( PackageName (..), Version (..), PackageId (..)
  , PackageRef (..), parsePackageRef )

-- | Errors specific to the Cabal build environment.
data CabalStoreError
  = StoreNotFound !FilePath
    -- ^ The cabal store directory does not exist.
  | GhcVersionUnknown
    -- ^ Could not determine GHC version from the store path.
  deriving stock (Show, Eq)

-- | Create a 'BuildEnv' backed by the Cabal store at @~/.cabal/store/ghc-X.Y.Z/@.
--
--   The GHC version is extracted from the store directory name.
mkCabalBuildEnv :: FilePath -> IO (Either CabalStoreError (BuildEnv IO))
mkCabalBuildEnv storeRoot = do
  exists <- doesDirectoryExist storeRoot
  if exists
    then do
      ghcVer <- detectGhcVersion storeRoot
      case ghcVer of
        Nothing -> pure (Left GhcVersionUnknown)
        Just ver -> do
          packagesRoot <- resolvePackagesRoot storeRoot
          pure (Right BuildEnv
            { discoverInstalledPackages = discoverInStore storeRoot
            , locatePackageSource       = locateSource storeRoot
            , locateRepoTarball         = locateRepoTarballAt packagesRoot
            , locateHaddockHtml         = locateHaddock storeRoot
            , ghcVersion                = pure ver
            })
    else pure (Left (StoreNotFound storeRoot))

-- | Detect GHC version from the store directory name.
--   Expects format: @ghc-X.Y.Z@
detectGhcVersion :: FilePath -> IO (Maybe Version)
detectGhcVersion storeRoot = do
  let dirname = takeFileName storeRoot
  case parseGhcDirName dirname of
    Nothing -> pure Nothing
    Just ver -> pure (Just (Version (Text.pack ver)))
  where
    parseGhcDirName :: String -> Maybe String
    parseGhcDirName name
      | take 4 name == "ghc-" = Just (drop 4 name)
      | otherwise = Nothing

-- | Discover all installed packages in the cabal store.
discoverInStore :: FilePath -> IO (Set PackageId)
discoverInStore storeRoot = do
  result <- try @IO @IOException (listDirectory storeRoot)
  case result of
    Left _  -> pure Set.empty
    Right entries -> do
      let pkgIds = mapMaybe parseStoreEntry entries
      pure (Set.fromList pkgIds)

-- | Parse a store directory entry into a 'PackageId'.
--   Store entries have format: @<pkg-name>-<version>-<hash>@ where the hash
--   segment is mandatory and consists of at least eight hex-ish characters
--   (the real cabal-install hashes are 64 hex; we accept >= 8 to keep the
--   predicate cheap and tolerant of test fixtures).
--
--   Without the hash requirement, entries like @async-2.2.5@ would parse as
--   name=async, ver=2.2, hash=5 — silently wrong.
parseStoreEntry :: String -> Maybe PackageId
parseStoreEntry entry =
  case parsePackageRef (Text.pack entry) of
    PackageRef name@(PackageName n) (Just ver)
      | not (Text.null n)
      , entryHasHash entry
      -> Just (PackageId name ver)
    _ -> Nothing
  where
    -- Require the trailing hash segment to distinguish a real store
    -- entry from a stray @pkg-ver@ directory.  'parsePackageRef'
    -- already strips the hash; we just confirm the original string
    -- carried one.
    entryHasHash s = case Text.breakOnEnd "-" (Text.pack s) of
      (prefix, suffix) ->
        not (Text.null prefix)
          && Text.length suffix >= 8
          && Text.all isHexish suffix
    isHexish c =
      (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

-- | Locate the source directory for a package.
--
--   The cabal store does not normally keep package sources after a build;
--   we make a best-effort search through a few candidate layouts:
--
--   1. @<storeRoot>\/src\/<pkg>-<ver>@ (used by some unpacked fixtures and
--      by some experimental cabal-install builds);
--   2. @<storeRoot>\/<pkg>-<ver>-<hash>\/src@ inside the per-package store
--      entry (rarely populated but supported by older toolchains);
--   3. otherwise return 'Nothing' — the @module@ / @symbol@ commands will
--      then report an empty result, and the caller can drive them through
--      @dist-newstyle\/src@ of the active project (this requires the
--      project root, which is wired in via the dispatcher, not here).
locateSource :: FilePath -> PackageId -> IO (Maybe FilePath)
locateSource storeRoot pid@(PackageId (PackageName name) (Version ver)) = do
  let prefix = Text.unpack name <> "-" <> Text.unpack ver
      candidate1 = storeRoot </> "src" </> prefix
  exists1 <- doesDirectoryExist candidate1
  if exists1
    then pure (Just candidate1)
    else findInStoreEntry storeRoot pid
  where
    findInStoreEntry :: FilePath -> PackageId -> IO (Maybe FilePath)
    findInStoreEntry sr (PackageId (PackageName n) (Version v)) = do
      r <- try @IO @IOException (listDirectory sr)
      case r of
        Left _ -> pure Nothing
        Right entries -> do
          let want = Text.unpack n <> "-" <> Text.unpack v <> "-"
              hit  = find (isPrefixOf want) entries
          case hit of
            Nothing  -> pure Nothing
            Just e   -> do
              let srcDir = sr </> e </> "src"
              ok <- doesDirectoryExist srcDir
              pure (if ok then Just srcDir else Nothing)

-- | Locate cabal-install's @packages@ root for the active configuration.
--
-- Resolution order (matches cabal-install's own precedence):
--
--   1. @$CABAL_DIR/packages@ when @CABAL_DIR@ is set.
--   2. The @packages@ directory two levels above @storeRoot@ — works
--      for the default @~/.cabal/store/ghc-X.Y.Z@ layout and any other
--      configuration where store and packages share a parent.
--   3. @~/.cabal/packages@ as the final fallback.
--
-- Returns whichever candidate exists on disk; if none do, returns the
-- @$CABAL_DIR@-derived (or HOME-derived) path so the caller can still
-- record the expected location for diagnostics.
resolvePackagesRoot :: FilePath -> IO FilePath
resolvePackagesRoot storeRoot = do
  mCabalDir <- lookupEnv "CABAL_DIR"
  home      <- getHomeDirectory
  let cabalDirCandidate = fmap (</> "packages") mCabalDir
      siblingCandidate  = takeDirectory (takeDirectory storeRoot) </> "packages"
      homeCandidate     = home </> ".cabal" </> "packages"
      candidates        = maybe id (:) cabalDirCandidate
                            [siblingCandidate, homeCandidate]
  firstExisting homeCandidate candidates
  where
    firstExisting fallback []     = pure fallback
    firstExisting fallback (p:ps) = do
      ok <- doesDirectoryExist p
      if ok then pure p else firstExisting fallback ps

-- | 'BuildEnv'-shaped wrapper around 'RepoCache.locateRepoTarball'.
-- An I/O failure on the packages root (e.g. EACCES) is announced on
-- @stderr@ and degraded to 'Nothing' so the resolver can still attempt
-- the network fallback — silent swallow is banned by CLAUDE.md, but a
-- hard failure here would needlessly break a recoverable resolve.
locateRepoTarballAt :: FilePath -> PackageId -> IO (Maybe FilePath)
locateRepoTarballAt packagesRoot pid = do
  r <- RepoCache.locateRepoTarball packagesRoot pid
  case r of
    Right m  -> pure m
    Left err -> do
      TIO.hPutStrLn stderr ("warning: " <> renderRepoLookupError err)
      pure Nothing

-- | Locate the Haddock HTML for a package.
--   Look in @share/doc/<pkg>-<ver>/index.html@ within the store entry.
locateHaddock :: FilePath -> PackageId -> IO (Maybe FilePath)
locateHaddock storeRoot (PackageId (PackageName name) (Version ver)) = do
  -- Find the store entry directory for this package
  result <- try @IO @IOException (listDirectory storeRoot)
  case result of
    Left _ -> pure Nothing
    Right entries -> do
      let prefix = Text.unpack name <> "-" <> Text.unpack ver
          matchingEntry = find (isPrefixOf prefix) entries
      case matchingEntry of
        Nothing -> pure Nothing
        Just entry -> do
          let indexPath = storeRoot </> entry </> "share" </> "doc" </> (Text.unpack name <> "-" <> Text.unpack ver) </> "index.html"
          exists <- doesFileExist indexPath
          if exists
            then pure (Just indexPath)
            else pure Nothing
