{-# LANGUAGE OverloadedStrings #-}
-- | HTTP download for package source tarballs.
--
-- The cache-first fallback chain (cabal-install repo cache → hypha
-- source cache → HTTP) lives in 'Hypha.Package.Resolver'.  This
-- module only handles the network step itself: an actual GET against
-- @hackage.haskell.org@, written to a temp file and extracted via the
-- pure-Haskell pipeline in 'Hypha.Cabal.RepoCache'.
module Hypha.Hackage.Source
  ( fetchAndExtractSource
  , enumerateSourceCache
  ) where

import Control.Exception (displayException)
import Control.Exception.Safe (try)
import qualified Data.ByteString.Lazy as LBS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Network.HTTP.Client
  ( HttpException, Response, httpLbs, newManager, parseRequest, requestHeaders
  , responseStatus, responseBody
  )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Header (hUserAgent)
import Network.HTTP.Types.Status (statusCode)
import System.Directory
  ( createDirectoryIfMissing, doesDirectoryExist, doesFileExist
  , listDirectory, removeFile )
import System.FilePath ((</>), takeDirectory)

import Hypha.Cabal.RepoCache (extractTarballGz)
import Hypha.Cache (sourceCacheRoot)
import Hypha.Hackage.Api (HackageClient (..), HackageError (..), sourceTarballUrl, userAgent)
import Hypha.Types.PackageId (PackageId (..))

-- | Download a source tarball from Hackage and extract it into
-- @destDir@.  This is the network-only path; callers that want the
-- cabal-install repo cache consulted first should go through
-- 'Hypha.Package.Resolver.resolveSrc' rather than calling here
-- directly.
fetchAndExtractSource
  :: HackageClient IO
  -> PackageId
  -> FilePath          -- ^ Destination directory
  -> IO (Either HackageError FilePath)
fetchAndExtractSource _hclient pid destDir = do
  let url = sourceTarballUrl pid
  mgr <- newManager tlsManagerSettings
  req <- parseRequest url
  let req' = req { requestHeaders = [(hUserAgent, userAgent)] }
  -- Only catch 'HttpException' here: it's what 'httpLbs' throws.
  -- Anything else (filesystem permission errors, async cancellation,
  -- ...) bubbles up to the top-level catchAny in @app/hypha/Main.hs@.
  result <- try (httpLbs req' mgr) :: IO (Either HttpException (Response LBS.ByteString))
  case result of
    Left ex -> pure (Left (NetworkError (displayException ex)))
    Right resp -> do
      let status = statusCode (responseStatus resp)
      if status >= 200 && status < 300
        then writeAndExtract (responseBody resp)
        else pure (Left (HttpError status))
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
enumerateSourceCache :: IO (Map FilePath FilePath)
enumerateSourceCache = do
  dir <- sourceCacheRoot
  ok  <- doesDirectoryExist dir
  if not ok
    then pure Map.empty
    else do
      entries <- listDirectory dir
      pure (Map.fromList [ (e, dir </> e) | e <- entries ])
