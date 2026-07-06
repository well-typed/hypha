{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Hackage.Cache
  ( CacheKey (..)
  , mkCacheKey
  , lookupCache
  , insertCache
  , isFresh
  ) where

import Control.Exception (displayException)
import Control.Exception.Safe (try, SomeException)
import Crypto.Hash.SHA256 (hash)
import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.Time (diffUTCTime, getCurrentTime)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)

import Hypha.Hackage.Types (CacheKind (..), CachedResponse (..), encodeBytesHex)

-- | SHA-256 hash of the URL, used as the cache key.  Equality on 'CacheKey'
-- is structural; two clients hitting the same URL agree on the file name.
newtype CacheKey = CacheKey { unCacheKey :: String }
  deriving stock (Show, Eq, Ord)
  deriving newtype (FromJSON, ToJSON)

-- | Compute a cache key from a URL string.
mkCacheKey :: String -> IO CacheKey
mkCacheKey url = do
  let urlBytes  = BS8.pack url
      hashBytes = hash urlBytes
  pure (CacheKey (encodeBytesHex hashBytes))

-- | File path for a given cache key.
cacheFilePath :: FilePath -> CacheKey -> FilePath
cacheFilePath cacheDir key = cacheDir </> unCacheKey key <> ".json"

-- | Look up a cached response.  Returns 'Nothing' on miss; corruption
-- (unreadable file or unparsable JSON) is also reported as a miss so
-- the caller refetches, but the underlying cause is announced on
-- stderr — never silently swallowed (see CLAUDE.md, "Never ignore an
-- error branch silently").
lookupCache :: FilePath -> CacheKey -> IO (Maybe CachedResponse)
lookupCache cacheDir key = do
  let path = cacheFilePath cacheDir key
  exists <- doesFileExist path
  if not exists
    then pure Nothing
    else do
      result <- try (BS.readFile path) :: IO (Either SomeException BS.ByteString)
      case result of
        Left e  -> do
          hPutStrLn stderr $
            "warning: Hackage cache read failed at " <> path
            <> "; treating as miss: " <> displayException e
          pure Nothing
        Right bs -> case eitherDecodeStrict bs of
          Left err -> do
            hPutStrLn stderr $
              "warning: Hackage cache corrupt at " <> path
              <> "; treating as miss: " <> err
            pure Nothing
          Right cr -> pure (Just cr)

-- | Insert (or replace) a response in the cache.  Creates the cache
-- directory if needed.
insertCache :: FilePath -> CacheKey -> CachedResponse -> IO ()
insertCache cacheDir key resp = do
  createDirectoryIfMissing True cacheDir
  let path = cacheFilePath cacheDir key
  LBS.writeFile path (encode resp)

-- | Check if a cached response is still fresh.  Immutable responses are
-- always fresh; mutable responses honour their TTL.
isFresh :: CachedResponse -> IO Bool
isFresh resp = do
  now <- getCurrentTime
  case crKind resp of
    Immutable      -> pure True
    TtlMutable ttl -> pure (diffUTCTime now (crStoredAt resp) < ttl)