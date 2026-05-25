{-# LANGUAGE OverloadedStrings #-}
-- | Download and extract package source tarballs from Hackage.
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
  ( createDirectoryIfMissing, doesDirectoryExist, listDirectory, removeFile )
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (system)

import Hypha.Cache (sourceCacheRoot)
import Hypha.Hackage.Api (HackageClient (..), HackageError (..), sourceTarballUrl, userAgent)
import Hypha.Types.PackageId (PackageId (..))

-- | Download and extract the source tarball for a package from Hackage.
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
        then do
          let body = responseBody resp
          createDirectoryIfMissing True destDir
          let tmpFile = destDir </> "source.tar.gz"
          LBS.writeFile tmpFile body
          ec <- system (unwords ["tar", "-xzf", tmpFile, "-C", destDir, "--strip-components=1"])
          removeFile tmpFile
          case ec of
            ExitSuccess   -> pure (Right destDir)
            ExitFailure c -> pure (Left (NetworkError ("tar extraction failed with code " ++ show c)))
        else pure (Left (HttpError status))

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
