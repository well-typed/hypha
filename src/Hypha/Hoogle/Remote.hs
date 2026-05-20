{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
-- | HTTP client for the public @hoogle.haskell.org@ service.
--
-- Results are cached in the @kv@ table of the global 'IndexCache' so
-- repeated queries within the TTL skip the network entirely.  The
-- transport is a record-of-functions so tests can inject a
-- deterministic stub without touching the network.
module Hypha.Hoogle.Remote
  ( RemoteError (..)
  , RemoteHoogleTransport (..)
  , RemoteOptions (..)
  , defaultRemoteOptions
  , searchRemote
  , searchRemoteWith
  , defaultTransport
  ) where

import Control.Exception (SomeException, try)
import qualified Crypto.Hash.SHA256 as SHA256
import Data.Aeson (FromJSON (..), eitherDecode, withObject, (.:?))
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Base16 as Base16
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Network.HTTP.Client
  ( Manager, Request (..), Response, httpLbs, newManager
  , parseRequest, responseBody, responseTimeoutMicro )
import Network.HTTP.Client.TLS (tlsManagerSettings)

import Hypha.Hoogle.Type (HoogleHit (..), HoogleQuery (..))
import Hypha.Search.Cache (readBlob, writeBlob)
import Hypha.Search.PackageCache (HyphaPackageCache, hyphaGlobalCache)

-- | Why a remote call did not produce results.
data RemoteError
  = RemoteOffline
  | RemoteTimeout
  | RemoteHttp !Text
  | RemoteDecode !Text
  deriving stock (Show, Eq)

-- | Injection point for the HTTP transport.
newtype RemoteHoogleTransport = RemoteHoogleTransport
  { runRemote :: Text -> IO (Either RemoteError ByteString)
  }

data RemoteOptions = RemoteOptions
  { roOffline       :: !Bool
  , roTtlSeconds    :: !Int
  , roTimeoutMicros :: !Int
  , roEndpoint      :: !Text
  }
  deriving stock (Show, Eq)

defaultRemoteOptions :: RemoteOptions
defaultRemoteOptions = RemoteOptions
  { roOffline       = False
  , roTtlSeconds    = 86400
  , roTimeoutMicros = 10_000_000
  , roEndpoint      = "https://hoogle.haskell.org"
  }

-- | Production wrapper: bakes in the TLS-aware transport.  The KV
-- cache currently does not honour TTL (the @kv@ table stores 'Text'
-- payloads only); refreshing across the TTL boundary is a known
-- limitation flagged in the spec — for now any cached entry wins.
searchRemote
  :: RemoteOptions
  -> HyphaPackageCache
  -> HoogleQuery
  -> IO (Either RemoteError [HoogleHit])
searchRemote opts cache q = do
  transport <- defaultTransport opts
  searchRemoteWith transport opts cache q

searchRemoteWith
  :: RemoteHoogleTransport
  -> RemoteOptions
  -> HyphaPackageCache
  -> HoogleQuery
  -> IO (Either RemoteError [HoogleHit])
searchRemoteWith transport opts cache q
  | roOffline opts = pure (Left RemoteOffline)
  | otherwise = do
      let kv  = hyphaGlobalCache cache
          key = cacheKey q
      cached <- readBlob kv key
      case cached of
        Just txt
          | Right hits <- decodeHits (LBS.fromStrict (Text.encodeUtf8 txt))
              -> pure (Right hits)
        _ -> do
          let url = endpointFor opts q
          r <- runRemote transport url
          case r of
            Left e     -> pure (Left e)
            Right body -> case decodeHits body of
              Right hits -> do
                writeBlob kv key (Text.decodeUtf8 (LBS.toStrict body))
                pure (Right hits)
              Left err -> pure (Left (RemoteDecode err))

endpointFor :: RemoteOptions -> HoogleQuery -> Text
endpointFor opts (HoogleQuery q) =
  roEndpoint opts
    <> "/?mode=json&count=20&hoogle="
    <> urlEncode q

urlEncode :: Text -> Text
urlEncode = Text.concatMap encChar
  where
    encChar c
      | c == ' ' = "+"
      | c `elem` ("&?#=" :: String) = Text.pack ('%' : hex c)
      | otherwise = Text.singleton c
    hex c = let n = fromEnum c
                d1 = n `div` 16
                d2 = n `mod` 16
            in [digit d1, digit d2]
    digit n | n < 10 = toEnum (fromEnum '0' + n)
            | otherwise = toEnum (fromEnum 'A' + n - 10)

cacheKey :: HoogleQuery -> Text
cacheKey (HoogleQuery q) =
  -- Bumped the prefix when the upstream JSON post-processing changed
  -- (HTML-entity decoding, tag stripping, sig split) so stale cache
  -- entries written by older binaries don't shadow the cleaner shape.
  "hoogle:remote:v2:"
  <> Text.decodeUtf8
       (Base16.encode (SHA256.hash (Text.encodeUtf8 q)))

-- | Raw JSON shape from the @?mode=json@ endpoint.
data RawHit = RawHit
  { rhPkg :: !Text
  , rhMod :: !Text
  , rhItm :: !Text
  , rhTyp :: !Text
  , rhDoc :: !Text
  }

instance FromJSON RawHit where
  parseJSON = withObject "RawHit" $ \o -> do
    pkg <- nameField o "package"
    md  <- nameField o "module"
    itm <- fromMaybe "" <$> o .:? "item"
    typ <- fromMaybe "" <$> o .:? "type"
    doc <- fromMaybe "" <$> o .:? "docs"
    pure (RawHit pkg md itm typ doc)
    where
      nameField o k = do
        mObj <- o .:? k
        case mObj of
          Nothing  -> pure ""
          Just obj -> fromMaybe "" <$> obj .:? "name"

decodeHits :: LBS.ByteString -> Either Text [HoogleHit]
decodeHits bs = case eitherDecode bs of
  Left err   -> Left (Text.pack err)
  Right hits -> Right (map fromRaw hits)
  where
    fromRaw r =
      -- Hoogle's @item@ field is HTML: wrapped @\<span class=name\>@,
      -- entities encoded.  Split on @ :: @ to recover (name, sig);
      -- when the upstream @type@ field is non-empty, prefer it.
      let cleanItem = decodeEntities (stripTags (rhItm r))
          cleanTyp  = decodeEntities (stripTags (rhTyp r))
          (name, sigFromItem) = splitSig cleanItem
          sig = if Text.null cleanTyp then sigFromItem else cleanTyp
      in HoogleHit (rhPkg r) (rhMod r) name sig (decodeEntities (rhDoc r))

-- | Drop every @\<…\>@ run.  Cheap; Hoogle's HTML payload is shallow
-- (no nested attributes with @\>@ inside).
stripTags :: Text -> Text
stripTags = go . Text.unpack
  where
    go []          = ""
    go ('<' : rs)  = go (drop 1 (dropWhile (/= '>') rs))
    go (c   : rs)  = Text.cons c (go rs)

-- | Tiny HTML-entity decoder covering only the entities Hoogle's
-- output actually emits (@&lt; &gt; &amp; &quot; &#39;@).  Numeric
-- entities beyond that are left alone — Hoogle does not use them
-- for the @item@ / @type@ fields.
decodeEntities :: Text -> Text
decodeEntities = Text.pack . go . Text.unpack
  where
    go [] = []
    go ('&':rest)
      | Just (c, rs) <- entity rest = c : go rs
    go (c:rs) = c : go rs

    entity s
      | Just rs <- prefix "lt;"   s = Just ('<',  rs)
      | Just rs <- prefix "gt;"   s = Just ('>',  rs)
      | Just rs <- prefix "amp;"  s = Just ('&',  rs)
      | Just rs <- prefix "quot;" s = Just ('"',  rs)
      | Just rs <- prefix "#39;"  s = Just ('\'', rs)
      | otherwise                   = Nothing

    prefix p s
      | take (length p) s == p = Just (drop (length p) s)
      | otherwise              = Nothing

-- | Split @\"id :: a -> a\"@ → @(\"id\", \"a -> a\")@.  When no
-- @ :: @ separator is present, the whole string is the name and the
-- signature is empty.
splitSig :: Text -> (Text, Text)
splitSig t = case Text.breakOn " :: " t of
  (name, rest)
    | Text.null rest -> (Text.strip name, "")
    | otherwise      -> (Text.strip name, Text.strip (Text.drop 4 rest))

-- | TLS-aware default transport.
defaultTransport :: RemoteOptions -> IO RemoteHoogleTransport
defaultTransport opts = do
  mgr <- newManager tlsManagerSettings
  pure (RemoteHoogleTransport (httpGet mgr opts))

httpGet
  :: Manager
  -> RemoteOptions
  -> Text
  -> IO (Either RemoteError ByteString)
httpGet mgr opts url = do
  reqE <- try (parseRequest (Text.unpack url))
            :: IO (Either SomeException Request)
  case reqE of
    Left e -> pure (Left (RemoteHttp (Text.pack (show e))))
    Right req0 -> do
      let req = req0 { responseTimeout =
                         responseTimeoutMicro (roTimeoutMicros opts) }
      r <- try (httpLbs req mgr)
             :: IO (Either SomeException (Response ByteString))
      case r of
        Left  e    -> pure (Left (RemoteHttp (Text.pack (show e))))
        Right resp -> pure (Right (responseBody resp))
