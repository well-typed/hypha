{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Hypha.Hackage.Api
  ( HackageClient (..)
  , PackageJson
  , OfflineMode (..)
  , HackageError (..)
  , mkHackageClient
  , mkOfflineHackageClient
    -- * Internals exposed for testing
  , userAgent
  , sourceTarballUrl
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar_)
import Control.Exception (try, SomeException)
import Data.Aeson (Value, decode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import Data.Time
  ( UTCTime, NominalDiffTime, diffUTCTime, getCurrentTime, secondsToNominalDiffTime
  , defaultTimeLocale, formatTime, parseTimeM, rfc822DateFormat
  )
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
import Network.HTTP.Types.Header
  ( hIfModifiedSince, hIfNoneMatch, hUserAgent, hETag, hLastModified )
import Network.HTTP.Types.Status (statusCode)

import Hypha.Types.PackageId (PackageId (..), PackageName (..), Version (..))
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

-- | User-Agent header value for hypha.  Includes the project contact email
-- (@info\@well-typed.com@) so Hackage admins can reach us if we ever
-- misbehave.  The version is hard-coded here rather than picked up from
-- @Paths_hypha@ for now; revisit if that becomes a maintenance burden.
userAgent :: BS.ByteString
userAgent = "hypha/0.0.0 (+https://github.com/well-typed/hypha; contact: info@well-typed.com)"

-- | Thread-safe rate limiter using 'MVar'.  The MVar holds the time of the
-- most recently completed request; throttle releases once at least
-- 'rlMinDelay' has elapsed.
data RateLimiter = RateLimiter
  { rlState    :: !(MVar (Maybe UTCTime))
  , rlMinDelay :: !NominalDiffTime
  }

newRateLimiter :: NominalDiffTime -> IO RateLimiter
newRateLimiter d = do
  m <- newMVar Nothing
  pure (RateLimiter m d)

throttle :: RateLimiter -> IO ()
throttle limiter = do
  now <- getCurrentTime
  modifyMVar_ (rlState limiter) $ \mLast -> do
    case mLast of
      Nothing -> pure (Just now)
      Just lastTime -> do
        let elapsed = diffUTCTime now lastTime
        if elapsed < rlMinDelay limiter
          then do
            let waitSec = realToFrac (rlMinDelay limiter - elapsed) :: Double
            threadDelay (floor (waitSec * 1e6))
            after <- getCurrentTime
            pure (Just after)
          else pure (Just now)

-- | Create an online HackageClient.
mkHackageClient :: Manager -> IO (HackageClient IO)
mkHackageClient manager = do
  limiter <- newRateLimiter (secondsToNominalDiffTime 1)
  pure HackageClient
    { fetchPackageJson = \pkgName -> do
        throttle limiter
        fetchPackageJsonOnline manager pkgName
    , fetchVersions = \pkgName -> do
        throttle limiter
        fetchVersionsOnline manager pkgName
    }

-- | Create an offline HackageClient that serves only from the on-disk cache.
-- A cache miss yields a typed 'OfflineCacheMiss' error.
mkOfflineHackageClient :: IO (HackageClient IO)
mkOfflineHackageClient = pure HackageClient
  { fetchPackageJson = \pkgName -> do
      let url = packageJsonUrl pkgName
      cacheKey <- Cache.mkCacheKey url
      mCached  <- Cache.lookupCache cacheKey
      case mCached of
        Nothing  -> pure (Left (OfflineCacheMiss pkgName))
        Just cr  -> pure (decodeJsonBody cr)
  , fetchVersions = \pkgName -> do
      let url = preferredVersionsUrl pkgName
      cacheKey <- Cache.mkCacheKey url
      mCached  <- Cache.lookupCache cacheKey
      case mCached of
        Nothing -> pure (Left (OfflineCacheMiss pkgName))
        Just cr -> pure (Right (parseVersions (crBody cr)))
  }

packageJsonUrl :: PackageName -> String
packageJsonUrl pkgName =
  "https://hackage.haskell.org/package/" ++ Text.unpack (unPackageName pkgName) ++ ".json"

preferredVersionsUrl :: PackageName -> String
preferredVersionsUrl pkgName =
  "https://hackage.haskell.org/package/" ++ Text.unpack (unPackageName pkgName) ++ "/preferred"

-- | URL for a package source tarball on Hackage.
sourceTarballUrl :: PackageId -> String
sourceTarballUrl (PackageId (PackageName n) (Version v)) =
  "https://hackage.haskell.org/package/"
    ++ Text.unpack n ++ "-" ++ Text.unpack v
    ++ "/" ++ Text.unpack n ++ "-" ++ Text.unpack v ++ ".tar.gz"

decodeJsonBody :: CachedResponse -> Either HackageError Value
decodeJsonBody cr = case decode (LBS.fromStrict (crBody cr)) of
  Just v  -> Right v
  Nothing -> Left (DecodeError "failed to decode cached package JSON")

-- | Fetch package JSON from Hackage with cache revalidation.
fetchPackageJsonOnline :: Manager -> PackageName -> IO (Either HackageError Value)
fetchPackageJsonOnline manager pkgName = do
  let url = packageJsonUrl pkgName
  cacheKey <- Cache.mkCacheKey url
  cached <- Cache.lookupCache cacheKey
  case cached of
    Nothing -> fetchAndCache manager url cacheKey (TtlMutable (secondsToNominalDiffTime 900)) Nothing Nothing decodeJsonBytes
    Just cr -> do
      fresh <- Cache.isFresh cr
      if fresh
        then pure (decodeJsonBody cr)
        else do
          mRefreshed <- revalidate manager url (crEtag cr) (crLastModified cr)
          case mRefreshed of
            Left e             -> pure (Left e)
            Right StillValid   -> do
              touchCache cacheKey cr
              pure (decodeJsonBody cr)
            Right (Refreshed bs et lm) -> do
              now <- getCurrentTime
              let cr' = CachedResponse et lm now bs (TtlMutable (secondsToNominalDiffTime 900))
              Cache.insertCache cacheKey cr'
              pure (decodeJsonBody cr')
  where
    decodeJsonBytes bs = case decode (LBS.fromStrict bs) of
      Just v  -> Right v
      Nothing -> Left (DecodeError "failed to decode package JSON")

-- | Fetch the @/preferred@ versions file.  Note: this file is mutable on
-- Hackage (a maintainer can change preferred-versions), so we treat it as a
-- TTL-bounded resource, not 'Immutable'.
fetchVersionsOnline :: Manager -> PackageName -> IO (Either HackageError [Version])
fetchVersionsOnline manager pkgName = do
  let url = preferredVersionsUrl pkgName
  cacheKey <- Cache.mkCacheKey url
  cached <- Cache.lookupCache cacheKey
  case cached of
    Nothing -> fetchAndCache manager url cacheKey (TtlMutable (secondsToNominalDiffTime 900)) Nothing Nothing
                 (Right . parseVersions)
    Just cr -> do
      fresh <- Cache.isFresh cr
      if fresh
        then pure (Right (parseVersions (crBody cr)))
        else do
          mRefreshed <- revalidate manager url (crEtag cr) (crLastModified cr)
          case mRefreshed of
            Left e             -> pure (Left e)
            Right StillValid   -> do
              touchCache cacheKey cr
              pure (Right (parseVersions (crBody cr)))
            Right (Refreshed bs et lm) -> do
              now <- getCurrentTime
              let cr' = CachedResponse et lm now bs (TtlMutable (secondsToNominalDiffTime 900))
              Cache.insertCache cacheKey cr'
              pure (Right (parseVersions bs))

-- | Outcome of an HTTP conditional GET.
data RevalResult
  = StillValid                                                -- ^ HTTP 304
  | Refreshed !BS.ByteString !(Maybe BS.ByteString) !(Maybe UTCTime)  -- ^ HTTP 200

-- | Perform a conditional GET, honouring 429/503 with backoff.  Uses
-- @If-None-Match@ and @If-Modified-Since@ — the correct request headers
-- for revalidation.
revalidate
  :: Manager
  -> String
  -> Maybe BS.ByteString
  -> Maybe UTCTime
  -> IO (Either HackageError RevalResult)
revalidate manager url mEtag mLastMod = do
  req <- parseRequest url
  let hdrs = (hUserAgent, userAgent)
           : maybe [] (\e -> [(hIfNoneMatch,    e)]) mEtag
          ++ maybe [] (\t -> [(hIfModifiedSince, formatHttpDate t)]) mLastMod
      req' = req { requestHeaders = hdrs }
  result <- try (httpLbs req' manager) :: IO (Either SomeException (Response LBS.ByteString))
  case result of
    Left ex -> pure (Left (NetworkError (show ex)))
    Right resp -> do
      let status = statusCode (responseStatus resp)
      case status of
        304 -> pure (Right StillValid)
        s | s >= 200 && s < 300 -> do
            let body = LBS.toStrict (responseBody resp)
                et   = lookup hETag         (responseHeaders resp)
                lm   = parseHttpDate =<< lookup hLastModified (responseHeaders resp)
            pure (Right (Refreshed body et lm))
        429 -> backoffAndRetry manager url mEtag mLastMod
        503 -> backoffAndRetry manager url mEtag mLastMod
        s   -> pure (Left (HttpError s))

backoffAndRetry
  :: Manager
  -> String
  -> Maybe BS.ByteString
  -> Maybe UTCTime
  -> IO (Either HackageError RevalResult)
backoffAndRetry manager url mEtag mLastMod = do
  threadDelay 2_000_000
  revalidate manager url mEtag mLastMod

-- | Fetch a URL fresh and store it in the cache; then decode.
fetchAndCache
  :: Manager
  -> String
  -> Cache.CacheKey
  -> CacheKind
  -> Maybe BS.ByteString
  -> Maybe UTCTime
  -> (BS.ByteString -> Either HackageError a)
  -> IO (Either HackageError a)
fetchAndCache manager url cacheKey kind _ _ decode_ = do
  r <- revalidate manager url Nothing Nothing
  case r of
    Left e              -> pure (Left e)
    Right StillValid    -> pure (Left (NetworkError "unexpected 304 on first fetch"))
    Right (Refreshed bs et lm) -> do
      now <- getCurrentTime
      let cr = CachedResponse et lm now bs kind
      Cache.insertCache cacheKey cr
      pure (decode_ bs)

-- | Persist a freshness-only touch (re-stamp 'storedAt' to defer the next
-- conditional GET).
touchCache :: Cache.CacheKey -> CachedResponse -> IO ()
touchCache key cr = do
  now <- getCurrentTime
  Cache.insertCache key (cr { crStoredAt = now })

-- | Parse versions from the preferred-versions file.  The real file is a
-- @cabal@-syntax constraint expression, but for alpha we just lift any
-- bare lines that look like version literals.  Lines starting with @--@
-- are comments; anything else is included verbatim.
parseVersions :: BS.ByteString -> [Version]
parseVersions bs =
  [ Version (Text.strip (Text.pack (BS8.unpack ln)))
  | ln <- BS8.split '\n' bs
  , not (BS.null ln)
  , not ("--" `BS.isPrefixOf` ln)
  ]

-- | Format a 'UTCTime' as an RFC 822 / HTTP-date string (e.g.
-- @"Wed, 21 Oct 2015 07:28:00 GMT"@).
formatHttpDate :: UTCTime -> BS.ByteString
formatHttpDate t =
  BS8.pack (formatTime defaultTimeLocale rfc822DateFormat t)

-- | Parse an RFC 822 / HTTP-date 'ByteString'.
parseHttpDate :: BS.ByteString -> Maybe UTCTime
parseHttpDate bs = parseTimeM True defaultTimeLocale rfc822DateFormat (BS8.unpack bs)
