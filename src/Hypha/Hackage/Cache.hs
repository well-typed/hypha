{-# LANGUAGE DerivingStrategies #-}
module Hypha.Hackage.Cache
  ( CacheKey (..)
  , mkCacheKey
  , lookupCache
  , insertCache
  , isFresh
  ) where

import Control.Exception (try, SomeException)
import Crypto.Hash.SHA256 (hash)
import Data.Aeson (FromJSON, ToJSON, encode, eitherDecodeStrict)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.Time (UTCTime, NominalDiffTime, diffUTCTime, getCurrentTime)
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))

import Hypha.Cache (hackageCacheDir)
import Hypha.Hackage.Types (CacheKind (..), CachedResponse (..), encodeBytesHex, decodeBytesHex)

-- | SHA-256 hash of the URL, used as the cache key.
newtype CacheKey = CacheKey { unCacheKey :: String }
  deriving stock (Show, Eq, Ord)
  deriving newtype (FromJSON, ToJSON)

-- | Compute a cache key from a URL string.
-- We hash the URL with SHA-256 and hex-encode it.
mkCacheKey :: String -> IO CacheKey
mkCacheKey url = do
  let urlBytes = BS8.pack url
  let hashBytes = hash urlBytes
  pure (CacheKey (encodeBytesHex hashBytes))

-- | JSON-serializable wrapper for persisting CachedResponse to disk.
-- Uses the same structure as CachedResponse with hex-encoded ByteStrings.
data CacheEntry = CacheEntry
  { ceEtag         :: !(Maybe String)
  , ceLastModified :: !(Maybe String)  -- ISO 8601 format
  , ceStoredAt     :: !String          -- ISO 8601 format
  , ceBody         :: !String          -- hex-encoded body
  , ceKind         :: !CacheKindDisk
  }
  deriving stock (Show, Eq, Generic)

data CacheKindDisk
  = DiskImmutable
  | DiskTtlMutable !NominalDiffTime
  deriving stock (Show, Eq, Generic)

instance FromJSON CacheKindDisk
instance ToJSON CacheKindDisk
instance FromJSON CacheEntry
instance ToJSON CacheEntry

-- | File path for a given cache key.
cacheFilePath :: CacheKey -> IO FilePath
cacheFilePath key = do
  dir <- hackageCacheDir
  pure (dir </> unCacheKey key <> ".json")

-- | Look up a cached response. Returns Nothing if not found or on read error.
lookupCache :: CacheKey -> IO (Maybe CachedResponse)
lookupCache key = do
  path <- cacheFilePath key
  exists <- doesFileExist path
  if exists
    then do
      result <- try (BS.readFile path) :: IO (Either SomeException BS.ByteString)
      case result of
        Left _ -> pure Nothing
        Right bs -> case eitherDecodeStrict bs of
          Left _ -> pure Nothing
          Right entry -> pure (diskToCached entry)
    else pure Nothing

-- | Insert a response into the cache. Creates directories as needed.
insertCache :: CacheKey -> CachedResponse -> IO ()
insertCache key resp = do
  dir <- hackageCacheDir
  createDirectoryIfMissing True dir
  path <- cacheFilePath key
  let entry = cachedToDisk resp
  LBS.writeFile path (encode entry)

-- | Check if a cached response is still fresh.
isFresh :: CachedResponse -> IO Bool
isFresh resp = do
  now <- getCurrentTime
  case crKind resp of
    Immutable -> pure True
    TtlMutable ttl -> do
      let age = diffUTCTime now (crStoredAt resp)
      pure (age < ttl)

-- | Convert from disk format to in-memory CachedResponse.
-- Returns Nothing if the stored time cannot be parsed (corrupted cache).
diskToCached :: CacheEntry -> Maybe CachedResponse
diskToCached entry = do
  stored <- parseUTCTime (ceStoredAt entry)
  body <- either (const Nothing) Just (decodeBytesHex (ceBody entry))
  pure CachedResponse
    { crEtag = fmap BS8.pack (ceEtag entry)
    , crLastModified = parseUTCTime =<< ceLastModified entry
    , crStoredAt = stored
    , crBody = body
    , crKind = diskToKind (ceKind entry)
    }

-- | Convert from in-memory CachedResponse to disk format.
cachedToDisk :: CachedResponse -> CacheEntry
cachedToDisk resp = CacheEntry
  { ceEtag = fmap BS8.unpack (crEtag resp)
  , ceLastModified = fmap showUTCTime (crLastModified resp)
  , ceStoredAt = showUTCTime (crStoredAt resp)
  , ceBody = encodeBytesHex (crBody resp)
  , ceKind = kindToDisk (crKind resp)
  }

diskToKind :: CacheKindDisk -> CacheKind
diskToKind DiskImmutable = Immutable
diskToKind (DiskTtlMutable ttl) = TtlMutable ttl

kindToDisk :: CacheKind -> CacheKindDisk
kindToDisk Immutable = DiskImmutable
kindToDisk (TtlMutable ttl) = DiskTtlMutable ttl

-- | Parse ISO 8601 UTCTime. Returns Nothing on parse failure.
parseUTCTime :: String -> Maybe UTCTime
parseUTCTime s = case reads s of
  [(t, "")] -> Just t
  _ -> Nothing

-- | Format UTCTime as ISO 8601.
showUTCTime :: UTCTime -> String
showUTCTime = show
