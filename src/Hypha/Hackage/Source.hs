{-# LANGUAGE OverloadedStrings #-}
-- | Download and extract package source tarballs from Hackage.
module Hypha.Hackage.Source
  ( fetchAndExtractSource
  ) where

import Control.Exception (IOException, try)
import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Client
  ( Response, httpLbs, newManager, parseRequest, requestHeaders
  , responseStatus, responseBody
  )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Header (hUserAgent)
import Network.HTTP.Types.Status (statusCode)
import System.Directory (createDirectoryIfMissing, removeFile)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (system)

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
  result <- try (httpLbs req' mgr) :: IO (Either IOException (Response LBS.ByteString))
  case result of
    Left ex -> pure (Left (NetworkError (show ex)))
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
