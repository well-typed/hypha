{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Lookup and extraction helpers for cabal-install's local repository
-- tarball cache.
--
-- cabal-install downloads every Hackage tarball into a tree of the form
-- @<packagesRoot>/<repo>/<pkg>/<ver>/<pkg>-<ver>.tar.gz@.  Reusing those
-- tarballs means hypha does not pay a second network round-trip the
-- first time it needs the source for an out-of-plan package.
--
-- This module is intentionally environment-agnostic: it takes the
-- packages root as a parameter rather than hard-coding @~/.cabal@.  The
-- 'Hypha.BuildEnv.BuildEnv' record is the layer that knows which
-- packages root applies to the current build environment, and exposes
-- the lookup via 'Hypha.BuildEnv.Type.locateRepoTarball'.
module Hypha.Cabal.RepoCache
  ( -- * Tarball discovery
    locateRepoTarball
  , RepoLookupError (..)
  , renderRepoLookupError
    -- * Tarball extraction
  , extractTarballGz
  , TarballError (..)
  , renderTarballError
  ) where

import Control.Exception.Safe (IOException, SomeException, try)
import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory
  ( createDirectoryIfMissing, doesDirectoryExist, doesFileExist
  , listDirectory, removeDirectoryRecursive, renameDirectory )
import System.FilePath ((</>), takeDirectory)

import Hypha.Types.PackageId (PackageId (..), PackageName (..), Version (..))

-- | Failure modes for locating a tarball under @<packagesRoot>@.  We
-- distinguish "the directory does not exist" (legitimate cache miss,
-- handled silently by callers) from "the directory exists but we
-- cannot read it" (a real I/O problem the user must see).
data RepoLookupError
  = RepoRootUnreadable !FilePath !String
    -- ^ @listDirectory@ failed for a reason other than ENOENT (typically
    -- EACCES); first field is the path, second is the exception message.
  deriving stock (Show, Eq)

-- | Failure modes for tarball extraction.
data TarballError
  = TarballMissing       !FilePath
    -- ^ Source @.tar.gz@ does not exist on disk.
  | TarballReadError     !FilePath !String
    -- ^ @LBS.readFile@ failed.  Carries path and exception message.
  | TarballExtractError  !FilePath !String
    -- ^ @tar@/@zlib@ rejected the archive.  Carries path and message.
  | TarballLayoutError   !FilePath !String
    -- ^ Archive decoded, but its layout was not the expected single
    -- top-level directory (e.g. multiple roots or a bare file).
  deriving stock (Show, Eq)

-- | User-facing renderer for 'TarballError'.  Call this at the wire
-- boundary only — never inside an error constructor.
renderTarballError :: TarballError -> Text
renderTarballError = \case
  TarballMissing p ->
    "local tarball missing: " <> Text.pack p
  TarballReadError p msg ->
    "tarball read failed for " <> Text.pack p <> ": " <> Text.pack msg
  TarballExtractError p msg ->
    "tarball extraction failed for " <> Text.pack p <> ": " <> Text.pack msg
  TarballLayoutError p msg ->
    "tarball " <> Text.pack p <> " has unexpected layout: " <> Text.pack msg

renderRepoLookupError :: RepoLookupError -> Text
renderRepoLookupError = \case
  RepoRootUnreadable p msg ->
    "could not list cabal repo cache at " <> Text.pack p <> ": " <> Text.pack msg

-- | Locate the tarball for a package under @packagesRoot@, if any
-- repository has it.  The first repo that has the tarball wins;
-- repository priority is cabal-install's concern at solve time, not
-- hypha's at lookup time.
--
-- Returns:
--
--   * @Right (Just path)@ — a repository had the tarball.
--   * @Right Nothing@      — no repository had the tarball, or
--                            @packagesRoot@ does not exist yet.
--   * @Left err@           — @packagesRoot@ exists but we could not
--                            read it; the caller should surface this
--                            rather than treating it as a cache miss.
locateRepoTarball
  :: FilePath        -- ^ packages root (e.g. @~/.cabal/packages@)
  -> PackageId
  -> IO (Either RepoLookupError (Maybe FilePath))
locateRepoTarball packagesRoot (PackageId (PackageName name) (Version ver)) = do
  rootExists <- doesDirectoryExist packagesRoot
  if not rootExists
    then pure (Right Nothing)
    else do
      reposE <- try @IO @IOException (listDirectory packagesRoot)
      case reposE of
        Left ex     -> pure (Left (RepoRootUnreadable packagesRoot (show ex)))
        Right repos -> Right <$> firstHit (map candidateFor repos)
  where
    nameS       = Text.unpack name
    verS        = Text.unpack ver
    tarballName = nameS <> "-" <> verS <> ".tar.gz"
    candidateFor repo =
      packagesRoot </> repo </> nameS </> verS </> tarballName

    firstHit :: [FilePath] -> IO (Maybe FilePath)
    firstHit []     = pure Nothing
    firstHit (p:ps) = do
      ok <- doesFileExist p
      if ok then pure (Just p) else firstHit ps

-- | Extract a @.tar.gz@ archive into a destination directory.
--
-- Decoded entirely in pure Haskell (no @tar@ or @gzip@ binary on
-- @PATH@).  Extraction targets a sibling @staging@ directory and is
-- renamed into place on success: a partial failure is rolled back and
-- never leaves a half-populated cache entry that a future cache-hit
-- check would mistake for a complete one.
--
-- The 'renameDirectory' step is best-effort atomic: it is safe against
-- crash mid-extraction, but two concurrent extractions of the same
-- package can race on the final rename (last writer wins).  Both
-- writers produce identical content, so the race is benign for
-- correctness; it can waste work.
--
-- The single top-level directory inside the tarball (the @pkg-ver/@
-- stanza that Hackage tarballs always carry) is stripped, matching
-- @tar --strip-components=1@.  An archive whose top level is not a
-- single directory yields 'TarballLayoutError'.
extractTarballGz
  :: FilePath       -- ^ source @.tar.gz@
  -> FilePath       -- ^ destination directory (will hold the stripped tree)
  -> IO (Either TarballError ())
extractTarballGz tarball destDir = do
  okSrc <- doesFileExist tarball
  if not okSrc
    then pure (Left (TarballMissing tarball))
    else do
      bytesE <- try @IO @SomeException (LBS.readFile tarball)
      case bytesE of
        Left ex     -> pure (Left (TarballReadError tarball (show ex)))
        Right bytes -> doExtract bytes
  where
    staging = destDir <> ".staging"

    doExtract bytes = do
      rmIfExists staging
      createDirectoryIfMissing True staging
      resE <- try @IO @SomeException
        (Tar.unpack staging (Tar.read (GZip.decompress bytes)))
      case resE of
        Left ex -> do
          rmIfExists staging
          pure (Left (TarballExtractError tarball (show ex)))
        Right () -> promote

    promote = do
      mInner <- findSingleSubdir staging
      case mInner of
        Nothing -> do
          rmIfExists staging
          pure (Left (TarballLayoutError tarball
            "expected a single top-level directory"))
        Just inner -> do
          rmIfExists destDir
          createDirectoryIfMissing True (takeDirectory destDir)
          renameDirectory inner destDir
          rmIfExists staging
          pure (Right ())

    rmIfExists d = do
      ok <- doesDirectoryExist d
      if ok then removeDirectoryRecursive d else pure ()

    findSingleSubdir root = do
      entries <- listDirectory root
      case entries of
        [only] -> do
          let p = root </> only
          isDir <- doesDirectoryExist p
          pure $ if isDir then Just p else Nothing
        _      -> pure Nothing
