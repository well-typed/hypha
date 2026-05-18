{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Hackage.Api
  ( HackageClient (..)
  , PackageJson
  , OfflineMode (..)
  , HackageError (..)
  , mkHackageClient
  , mkOfflineHackageClient
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (try, SomeException)
import Data.Aeson (FromJSON, ToJSON, Value, decode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime, NominalDiffTime, diffUTCTime, getCurrentTime, secondsToNominalDiffTime)
import Network.HTTP.Client
  ( Manager
  , Response
  , httpLbs
  , parseRequest
  , requestHeaders
  , responseStatus
  , responseBody
  , responseHeaders
  )
import Network.HTTP.Types.Header (hETag, hLastModified, hUserAgent)
import Network.HTTP.Types.Status (statusCode)

import Hypha.Types.PackageId (PackageName (..), Version (..))
import qualified Hypha.Hackage.Cache as Cache
import Hypha.Hackage.Types (CacheKind (..), CachedResponse (..))

-- | Type alias for package JSON (opaque for now).
type PackageJson = Value

-- | Whether the client operates in offline mode.
data OfflineMode = Online | Offline
  deriving stock (Show, Eq)

-- | A HackageClient provides methods to fetch package metadata.
data HackageClient m = HackageClient
  { fetchPackageJson :: PackageName -> m (Either HackageError Value)
  , fetchVersions    :: PackageName -> m (Either HackageError [Version])
  }

-- | Errors that can occur when fetching from Hackage.
data HackageError
  = NetworkError !String
  | OfflineCacheMiss !PackageName
  | DecodeError !String
  | HttpError !Int
  deriving stock (Show, Eq)

-- | User-Agent header value for hypha.
userAgent :: BS.ByteString
userAgent = "hypha/0.0.0 (+https://github.com/well-typed/hypha; contact: info@well-typed.com)"

-- | Mutable rate limiter state.
data RateLimiter = RateLimiter
  { rlLastRequest :: !(IORef (Maybe UTCTime))
  , rlMinDelay    :: !NominalDiffTime
  }

-- | Create a new rate limiter.
newRateLimiter :: IO RateLimiter
newRateLimiter = do
  ref <- newIORef Nothing
  pure RateLimiter
    { rlLastRequest = ref
    , rlMinDelay = secondsToNominalDiffTime 1
    }

-- | Wait if necessary to respect rate limits.
throttle :: RateLimiter -> IO ()
throttle limiter = do
  now <- getCurrentTime
  lastReq <- readIORef (rlLastRequest limiter)
  case lastReq of
    Nothing -> writeIORef (rlLastRequest limiter) (Just now)
    Just lastTime -> do
      let elapsed = diffUTCTime now lastTime
      if elapsed < rlMinDelay limiter
        then do
          let delaySeconds = realToFrac (rlMinDelay limiter - elapsed)
          threadDelay (floor (delaySeconds * 1000000))
          writeIORef (rlLastRequest limiter) (Just now)
        else writeIORef (rlLastRequest limiter) (Just now)

-- | Create an online HackageClient.
mkHackageClient :: Manager -> IO (HackageClient IO)
mkHackageClient manager = do
  limiter <- newRateLimiter
  pure HackageClient
    { fetchPackageJson = \pkgName -> do
        throttle limiter
        fetchPackageJsonOnline manager pkgName
    , fetchVersions = \pkgName -> do
        throttle limiter
        fetchVersionsOnline manager pkgName
    }

-- | Create an offline HackageClient that only uses the cache.
mkOfflineHackageClient :: IO (HackageClient IO)
mkOfflineHackageClient = pure HackageClient
  { fetchPackageJson = \pkgName ->
      pure (Left (OfflineCacheMiss pkgName))
  , fetchVersions = \pkgName ->
      pure (Left (OfflineCacheMiss pkgName))
  }

-- | Fetch package JSON from Hackage with cache revalidation.
fetchPackageJsonOnline :: Manager -> PackageName -> IO (Either HackageError Value)
fetchPackageJsonOnline manager pkgName = do
  let url = "https://hackage.haskell.org/package/" ++ Text.unpack (unPackageName pkgName) ++ ".json"
  cacheKey <- Cache.mkCacheKey url
  cached <- Cache.lookupCache cacheKey
  case cached of
    Nothing -> do
      result <- fetchWithRetry manager url Nothing Nothing
      case result of
        Left err -> pure (Left err)
        Right (body, etag, lastMod) -> do
          now <- getCurrentTime
          let resp = CachedResponse
                { crEtag = etag
                , crLastModified = lastMod
                , crStoredAt = now
                , crBody = body
                , crKind = TtlMutable (secondsToNominalDiffTime 900)
                }
          Cache.insertCache cacheKey resp
          case decode (LBS.fromStrict body) of
            Just val -> pure (Right val)
            Nothing -> pure (Left (DecodeError "failed to decode package JSON"))
    Just cachedResp -> do
      fresh <- Cache.isFresh cachedResp
      if fresh
        then case decode (LBS.fromStrict (crBody cachedResp)) of
          Just val -> pure (Right val)
          Nothing -> pure (Left (DecodeError "failed to decode cached package JSON"))
        else do
          result <- fetchWithRetry manager url (crEtag cachedResp) (crLastModified cachedResp)
          case result of
            Left err -> pure (Left err)
            Right (body, etag, lastMod) -> do
              now <- getCurrentTime
              let newResp = CachedResponse
                    { crEtag = etag
                    , crLastModified = lastMod
                    , crStoredAt = now
                    , crBody = body
                    , crKind = TtlMutable (secondsToNominalDiffTime 900)
                    }
              Cache.insertCache cacheKey newResp
              case decode (LBS.fromStrict body) of
                Just val -> pure (Right val)
                Nothing -> pure (Left (DecodeError "failed to decode revalidated package JSON"))

-- | Fetch version list from Hackage.
fetchVersionsOnline :: Manager -> PackageName -> IO (Either HackageError [Version])
fetchVersionsOnline manager pkgName = do
  let url = "https://hackage.haskell.org/package/" ++ Text.unpack (unPackageName pkgName) ++ "/preferred"
  cacheKey <- Cache.mkCacheKey url
  cached <- Cache.lookupCache cacheKey
  case cached of
    Nothing -> do
      result <- fetchWithRetry manager url Nothing Nothing
      case result of
        Left err -> pure (Left err)
        Right (body, etag, lastMod) -> do
          now <- getCurrentTime
          let resp = CachedResponse
                { crEtag = etag
                , crLastModified = lastMod
                , crStoredAt = now
                , crBody = body
                , crKind = Immutable
                }
          Cache.insertCache cacheKey resp
          pure (Right (parseVersions body))
    Just cachedResp -> do
      fresh <- Cache.isFresh cachedResp
      if fresh
        then pure (Right (parseVersions (crBody cachedResp)))
        else do
          result <- fetchWithRetry manager url (crEtag cachedResp) (crLastModified cachedResp)
          case result of
            Left err -> pure (Left err)
            Right (body, etag, lastMod) -> do
              now <- getCurrentTime
              let newResp = CachedResponse
                    { crEtag = etag
                    , crLastModified = lastMod
                    , crStoredAt = now
                    , crBody = body
                    , crKind = Immutable
                    }
              Cache.insertCache cacheKey newResp
              pure (Right (parseVersions body))

-- | Parse versions from the preferred versions file.
-- The format is one version per line.
parseVersions :: BS.ByteString -> [Version]
parseVersions bs =
  map (Version . Text.pack . BS8.unpack)
      (filter (not . BS.null) (BS8.split '\n' bs))

-- | Fetch with retry and ETag/Last-Modified revalidation.
fetchWithRetry :: Manager -> String -> Maybe BS.ByteString -> Maybe UTCTime -> IO (Either HackageError (BS.ByteString, Maybe BS.ByteString, Maybe UTCTime))
fetchWithRetry manager url etag lastMod = do
  req <- parseRequest url
  let req' = req
        { requestHeaders =
            [ (hUserAgent, userAgent)
            ] ++ maybe [] (\e -> [(hETag, e)]) etag
              ++ maybe [] (\t -> [(hLastModified, bsShow t)]) lastMod
        }
  result <- try (httpLbs req' manager) :: IO (Either SomeException (Response LBS.ByteString))
  case result of
    Left ex -> pure (Left (NetworkError (show ex)))
    Right resp -> do
      let status = statusCode (responseStatus resp)
      if status == 304
        then do
          -- 304 Not Modified: cache is still valid, update storedAt
          now <- getCurrentTime
          let updatedResp = CachedResponse
                { crEtag = etag
                , crLastModified = lastMod
                , crStoredAt = now
                , crBody = BS.empty  -- body not needed for 304
                , crKind = TtlMutable (secondsToNominalDiffTime 900)
                }
          -- Re-insert with updated timestamp to refresh TTL
          cacheKey <- Cache.mkCacheKey url
          Cache.insertCache cacheKey updatedResp
          pure (Left (NetworkError "304 not modified - cache refreshed"))
        else if status >= 200 && status < 300
          then do
            let body = BS.concat . LBS.toChunks $ responseBody resp
            let newEtag = lookup hETag (responseHeaders resp)
            let newLastMod = lookup hLastModified (responseHeaders resp)
            pure (Right (body, newEtag, parseBSUTCTime =<< newLastMod))
          else if status == 429 || status == 503
            then do
              threadDelay 2000000
              fetchWithRetry manager url etag lastMod
            else pure (Left (HttpError status))

-- | Parse UTCTime from ByteString (HTTP date format).
parseBSUTCTime :: BS.ByteString -> Maybe UTCTime
parseBSUTCTime bs = case reads (BS8.unpack bs) of
  [(t, "")] -> Just t
  _ -> Nothing

-- | Show UTCTime as ByteString.
bsShow :: UTCTime -> BS.ByteString
bsShow = BS8.pack . show
