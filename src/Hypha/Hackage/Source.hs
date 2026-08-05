{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Unpacking package source tarballs.
--
-- The cache-first fallback chain (cabal-install repo cache → hypha
-- source cache → Hackage) lives in 'Hypha.Package.Resolver'.  This
-- module only handles the last step: bytes from the 'HackageClient',
-- written to a temp file and extracted via the pure-Haskell pipeline in
-- 'Hypha.Cabal.RepoCache'.  Whether those bytes may be fetched at all is
-- the client's business, not this module's.
module Hypha.Hackage.Source
  ( fetchAndExtractSource
  , enumerateSourceCache
  ) where

import qualified Data.ByteString.Lazy as LBS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import System.Directory
  ( createDirectoryIfMissing, doesDirectoryExist, doesFileExist
  , listDirectory, removeFile )
import System.FilePath ((</>), takeDirectory)

import Hypha.Cabal.RepoCache (extractTarballGz)
import Hypha.Hackage.Api (HackageClient (..), HackageError (..))
import Hypha.Types.PackageId (PackageId (..))

-- | Extract a package's source tarball into @destDir@, asking the client
-- for the bytes.  Callers that want the cabal-install repo cache consulted
-- first should go through 'Hypha.Package.Resolver.resolveSrc' rather than
-- calling here directly.
--
-- The bytes come from 'fetchSourceTarball' rather than from a 'Manager'
-- built here, which is what makes @--offline@ mean anything on this path:
-- this function used to take a client, bind it to @_hclient@, and download
-- regardless, so an offline invocation with an empty cache fetched
-- @base@ and @ghc-internal@ from Hackage and reported success. An offline
-- client now refuses in the one place the decision belongs.
fetchAndExtractSource
  :: HackageClient IO
  -> PackageId
  -> FilePath          -- ^ Destination directory
  -> IO (Either HackageError FilePath)
fetchAndExtractSource hclient pid destDir =
  fetchSourceTarball hclient pid >>= \case
    Left err   -> pure (Left err)
    Right body -> writeAndExtract body
  where
    writeAndExtract body = do
      createDirectoryIfMissing True (takeDirectory destDir)
      let tmpFile = destDir <> ".tar.gz"
      LBS.writeFile tmpFile body
      r <- extractTarballGz tmpFile destDir
      removeFileIfExists tmpFile
      case r of
        Right ()  -> pure (Right destDir)
        Left tErr -> pure (Left (TarballFailure tErr))

    removeFileIfExists p = do
      ok <- doesFileExist p
      if ok then removeFile p else pure ()

-- | List every cached package-source directory under
-- @$XDG_CACHE_HOME/hypha/source/@.  Each entry is keyed by the
-- @\"pkg-ver\"@ directory name and maps to the absolute path on disk.
-- Returns 'Map.empty' when the cache directory doesn't exist yet.
enumerateSourceCache :: FilePath -> IO (Map FilePath FilePath)
enumerateSourceCache sourceCache = do
  ok  <- doesDirectoryExist sourceCache
  if not ok
    then pure Map.empty
    else do
      entries <- listDirectory sourceCache
      pure (Map.fromList [ (e, sourceCache </> e) | e <- entries ])
