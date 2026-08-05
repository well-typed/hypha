{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Hypha.Hackage.Api
  ( HackageClient (..)
  , PackageJson
  , OfflineMode (..)
  , HackageError (..)
  , renderHackageError
  , mkHackageClient
  , mkOfflineHackageClient
    -- * Internals exposed for testing
  , userAgent
  , sourceTarballUrl
  , packageJsonUrl
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar_)
import Control.Exception (displayException)
import Control.Exception.Safe (try)
import Data.Aeson (Value, decode)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time
  ( UTCTime, NominalDiffTime, diffUTCTime, getCurrentTime, secondsToNominalDiffTime
  , defaultTimeLocale, formatTime, parseTimeM, rfc822DateFormat
  )
import Network.HTTP.Client
  ( HttpException
  , Manager
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

import Hypha.Cabal.RepoCache (TarballError, renderTarballError)
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
  , fetchSourceTarball :: PackageId -> m (Either HackageError LBS.ByteString)
    -- ^ The bytes of a package's source tarball, from Hackage.
    --
    -- A field of the client rather than a free function, because
    -- @--offline@ is expressed by /which client was built/ and nothing
    -- else.  'Hypha.Hackage.Source.fetchAndExtractSource' used to take a
    -- client, ignore it, and build its own 'Manager' — so an offline
    -- invocation downloaded anyway, and no amount of care at the call site
    -- could have stopped it.  Here the offline client simply has no way to
    -- reach the network, which is the only version of this that stays true.
  }

-- | Errors that can occur when fetching from Hackage.
--
-- Structured — callers wrap this in 'Hypha.Error.HackageFailure' so the
-- variant survives all the way to the wire layer.  Render at the edge
-- via 'renderHackageError'; never @show@ a value of this type into an
-- error message.
data HackageError
  = NetworkError !String
    -- ^ Transport-layer failure (TLS handshake, socket close, etc.).
  | OfflineCacheMiss !PackageName
    -- ^ @--offline@: package not in the local cache.
  | DecodeError !String
    -- ^ JSON decode failed.
  | HttpError !Int
    -- ^ Non-2xx HTTP status code.
  | MissingField !Text
    -- ^ JSON decode succeeded, but a required field was absent (carries
    --   the field name).
  | TarballFailure !TarballError
    -- ^ A local @.tar.gz@ (cabal repo cache or freshly downloaded) was
    -- present but could not be turned into an unpacked source tree.
    -- Carries the structured cause so the wire layer can distinguish
    -- @missing@/@read@/@extract@/@layout@ failures.
  deriving stock (Show, Eq)

-- | User-facing renderer for 'HackageError'.  Only call this at the
-- wire boundary (envelope message, stderr) — never inside an error
-- constructor.
renderHackageError :: PackageName -> HackageError -> Text
renderHackageError (PackageName name) = \case
  NetworkError msg ->
    "Hackage transport error for '" <> name <> "': " <> Text.pack msg
  OfflineCacheMiss _ ->
    "package '" <> name
      <> "' not cached; can't fetch from Hackage in offline mode"
  DecodeError msg ->
    "Hackage decode error for '" <> name <> "': " <> Text.pack msg
  HttpError code ->
    "Hackage HTTP " <> Text.pack (show code) <> " for '" <> name <> "'"
  MissingField fld ->
    "Hackage response for '" <> name <> "' lacked '" <> fld <> "' field"
  TarballFailure tErr ->
    "local cache failure for '" <> name <> "': " <> renderTarballError tErr

-- | User-Agent header value for hypha.  Includes the project contact email
-- (@info\@well-typed.com@) so Hackage admins can reach us if we ever
-- misbehave.  The version is hard-coded here rather than picked up from
-- @Paths_hypha@ for now; revisit if that becomes a maintenance burden.
userAgent :: BS.ByteString
userAgent = "hypha/0.1.0 (+https://github.com/well-typed/hypha; contact: info@well-typed.com)"

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
mkHackageClient :: Manager -> FilePath -> IO (HackageClient IO)
mkHackageClient manager cacheDir = do
  limiter <- newRateLimiter (secondsToNominalDiffTime 1)
  pure HackageClient
    { fetchPackageJson = \pkgName -> do
        throttle limiter
        fetchPackageJsonOnline manager cacheDir pkgName
    , fetchVersions = \pkgName -> do
        throttle limiter
        fetchVersionsOnline manager cacheDir pkgName
      -- Rate-limited like the metadata calls: a cold cross-package chain
      -- can ask for a tarball, and Hackage is a shared service.
    , fetchSourceTarball = \pid -> do
        throttle limiter
        fetchSourceTarballOnline manager pid
    }

-- | Create an offline HackageClient that serves only from the on-disk cache.
-- A cache miss yields a typed 'OfflineCacheMiss' error.
--
-- 'fetchVersions' reads back the same @\<pkg\>.json@ cache entry that
-- 'fetchPackageJson' (online or offline) populates, so a package fetched in
-- one mode is available to the other under @--offline@.
mkOfflineHackageClient :: FilePath -> IO (HackageClient IO)
mkOfflineHackageClient cacheDir = pure HackageClient
  { fetchPackageJson = fetchPackageJsonOffline cacheDir
  , fetchVersions = \pkgName ->
      fmap extractVersionList <$> fetchPackageJsonOffline cacheDir pkgName
    -- There is no cache of tarball /bytes/ to serve from: an extracted
    -- tree is what gets kept, and the resolver has already looked there
    -- before it reaches the client.  So this is a refusal, not a miss to
    -- fall back from -- and saying which package could not be had is what
    -- lets the caller name it.
  , fetchSourceTarball = \pid -> pure (Left (OfflineCacheMiss (pkgName pid)))
  }

fetchPackageJsonOffline :: FilePath -> PackageName -> IO (Either HackageError Value)
fetchPackageJsonOffline cacheDir pkgName = do
  let url = packageJsonUrl pkgName
  cacheKey <- Cache.mkCacheKey url
  mCached  <- Cache.lookupCache cacheDir cacheKey
  case mCached of
    Nothing -> pure (Left (OfflineCacheMiss pkgName))
    Just cr -> pure (decodeJsonBody cr)

packageJsonUrl :: PackageName -> String
packageJsonUrl pkgName =
  "https://hackage.haskell.org/package/" ++ Text.unpack (unPackageName pkgName) ++ ".json"

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
fetchPackageJsonOnline :: Manager -> FilePath -> PackageName -> IO (Either HackageError Value)
fetchPackageJsonOnline manager cacheDir pkgName = do
  let url = packageJsonUrl pkgName
  cacheKey <- Cache.mkCacheKey url
  cached <- Cache.lookupCache cacheDir cacheKey
  case cached of
    Nothing -> fetchAndCache manager cacheDir url cacheKey (TtlMutable (secondsToNominalDiffTime 900)) Nothing Nothing decodeJsonBytes
    Just cr -> do
      fresh <- Cache.isFresh cr
      if fresh
        then pure (decodeJsonBody cr)
        else do
          mRefreshed <- revalidate manager url (crEtag cr) (crLastModified cr)
          case mRefreshed of
            Left e             -> pure (Left e)
            Right StillValid   -> do
              touchCache cacheDir cacheKey cr
              pure (decodeJsonBody cr)
            Right (Refreshed bs et lm) -> do
              now <- getCurrentTime
              let cr' = CachedResponse et lm now bs (TtlMutable (secondsToNominalDiffTime 900))
              Cache.insertCache cacheDir cacheKey cr'
              pure (decodeJsonBody cr')
  where
    decodeJsonBytes bs = case decode (LBS.fromStrict bs) of
      Just v  -> Right v
      Nothing -> Left (DecodeError "failed to decode package JSON")

-- | Fetch the full list of versions available on Hackage by reading the
-- package's JSON metadata (the @{name}.json@ endpoint exposes a
-- @normal@/@deprecated@ map keyed by version).  This is more reliable than
-- the legacy @/preferred@ text file, which a maintainer may not have set.
fetchVersionsOnline :: Manager -> FilePath -> PackageName -> IO (Either HackageError [Version])
fetchVersionsOnline manager cacheDir pkgName = do
  result <- fetchPackageJsonOnline manager cacheDir pkgName
  pure (fmap extractVersionList result)

-- | Download a source tarball's bytes.
--
-- Not cached here: the caller extracts these into a source tree it keeps,
-- and holding the tarball as well would be the same package twice on disk.
fetchSourceTarballOnline
  :: Manager -> PackageId -> IO (Either HackageError LBS.ByteString)
fetchSourceTarballOnline manager pid = do
  req <- parseRequest (sourceTarballUrl pid)
  let req' = req { requestHeaders = [(hUserAgent, userAgent)] }
  -- Only 'HttpException', which is what 'httpLbs' throws; anything else
  -- (permissions, async cancellation) belongs to the top-level handler.
  result <- try (httpLbs req' manager)
              :: IO (Either HttpException (Response LBS.ByteString))
  pure $ case result of
    Left ex    -> Left (NetworkError (displayException ex))
    Right resp ->
      let status = statusCode (responseStatus resp)
      in if status >= 200 && status < 300
           then Right (responseBody resp)
           else Left (HttpError status)

-- | Pull every version key out of a package.json response.  Hackage's
-- @\<pkg\>.json@ is a flat @{ "0.6.7": "normal", "0.7": "deprecated" }@
-- map.  We return all versions in descending semantic order (newest first).
extractVersionList :: Value -> [Version]
extractVersionList = \case
  Aeson.Object obj ->
    map Version $ sortDesc [ Key.toText k | k <- KM.keys obj ]
  _ -> []
  where
    sortDesc :: [Text.Text] -> [Text.Text]
    sortDesc = reverse . foldr insertAsc []

    insertAsc :: Text.Text -> [Text.Text] -> [Text.Text]
    insertAsc x []     = [x]
    insertAsc x (y:ys)
      | compareVersion x y == LT = x : y : ys
      | otherwise                = y : insertAsc x ys

    compareVersion :: Text.Text -> Text.Text -> Ordering
    compareVersion a b = compare (parseSegments a) (parseSegments b)

    parseSegments :: Text.Text -> [Int]
    parseSegments t =
      [ readInt s | s <- map Text.unpack (Text.splitOn (Text.pack ".") t) ]

    readInt :: String -> Int
    readInt s = case reads s of
      [(n, "")] -> n
      _         -> 0

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
  -- Narrow to 'HttpException' — what 'httpLbs' actually throws.
  -- Anything else bubbles to the top-level catchAny in Main.
  result <- try (httpLbs req' manager) :: IO (Either HttpException (Response LBS.ByteString))
  case result of
    Left ex -> pure (Left (NetworkError (displayException ex)))
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
  -> FilePath
  -> String
  -> Cache.CacheKey
  -> CacheKind
  -> Maybe BS.ByteString
  -> Maybe UTCTime
  -> (BS.ByteString -> Either HackageError a)
  -> IO (Either HackageError a)
fetchAndCache manager cacheDir url cacheKey kind _ _ decode_ = do
  r <- revalidate manager url Nothing Nothing
  case r of
    Left e              -> pure (Left e)
    Right StillValid    -> pure (Left (NetworkError "unexpected 304 on first fetch"))
    Right (Refreshed bs et lm) -> do
      now <- getCurrentTime
      let cr = CachedResponse et lm now bs kind
      Cache.insertCache cacheDir cacheKey cr
      pure (decode_ bs)

-- | Persist a freshness-only touch (re-stamp 'storedAt' to defer the next
-- conditional GET).
touchCache :: FilePath -> Cache.CacheKey -> CachedResponse -> IO ()
touchCache cacheDir key cr = do
  now <- getCurrentTime
  Cache.insertCache cacheDir key (cr { crStoredAt = now })

-- | Format a 'UTCTime' as an RFC 822 / HTTP-date string (e.g.
-- @"Wed, 21 Oct 2015 07:28:00 GMT"@).
formatHttpDate :: UTCTime -> BS.ByteString
formatHttpDate t =
  BS8.pack (formatTime defaultTimeLocale rfc822DateFormat t)

-- | Parse an RFC 822 / HTTP-date 'ByteString'.
parseHttpDate :: BS.ByteString -> Maybe UTCTime
parseHttpDate bs = parseTimeM True defaultTimeLocale rfc822DateFormat (BS8.unpack bs)
