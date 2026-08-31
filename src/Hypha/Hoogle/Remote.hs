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
  , renderRemoteError
  , RemoteHoogleTransport (..)
  , RemoteOptions (..)
  , defaultRemoteOptions
  , searchRemote
  , searchRemoteWith
  , cacheKey
  , defaultTransport
  ) where

import Control.Exception (displayException)
import Control.Exception.Safe (try)
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
  ( HttpException, Manager, Request (..), Response, httpLbs, newManager
  , parseRequest, responseBody, responseTimeoutMicro )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.URI (renderQuery)

import Hypha.Hoogle.Format
  ( decodeEntities, splitNameSig, stripTags )
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

-- | Total renderer for 'RemoteError'.  Lives next to the type so error
-- constructors can carry 'RemoteError' values directly (rather than
-- @Text.pack . show@ at the call site) and let the wire-format layer
-- decide how to surface them.
renderRemoteError :: RemoteError -> Text
renderRemoteError = \case
  RemoteOffline    -> "offline (remote Hoogle tier suppressed)"
  RemoteTimeout    -> "remote Hoogle timed out"
  RemoteHttp msg   -> "remote Hoogle HTTP error: " <> msg
  RemoteDecode msg -> "remote Hoogle response decode failure: " <> msg

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
-- The cache is consulted /before/ 'roOffline' is checked: an
-- "offline" run must still be answered by bodies already on disk
-- (the plane / CI / sandbox case), so 'RemoteOffline' means "offline
-- and nothing cached" — not "pretend tier 3 does not exist".
searchRemoteWith transport opts cache q = do
  let kv  = hyphaGlobalCache cache
      key = cacheKey q
  cached <- readBlob kv key
  case cached of
    Just txt
      | Right hits <- decodeHits (LBS.fromStrict (Text.encodeUtf8 txt))
          -> pure (Right hits)
    _
      | roOffline opts -> pure (Left RemoteOffline)
      | otherwise -> do
          let url = endpointFor opts q
          r <- runRemote transport url
          case r of
            Left e     -> pure (Left e)
            Right body -> case decodeHits body of
              Right hits -> do
                writeBlob kv key (Text.decodeUtf8 (LBS.toStrict body))
                pure (Right hits)
              Left err -> pure (Left (RemoteDecode err))

-- | Type-signature queries are full of characters that are not URL
-- syntax anywhere (@>@, @[@, @]@), so the query string is rendered by
-- 'renderQuery' rather than by hand: @parseRequest@ parses the URL
-- before it opens a socket, and an under-escaped one never reaches
-- Hoogle at all — it comes back as an @InvalidUrlException@.
endpointFor :: RemoteOptions -> HoogleQuery -> Text
endpointFor opts (HoogleQuery q) =
  roEndpoint opts
    <> "/"
    <> Text.decodeUtf8
         (renderQuery True
            [ ("mode",   Just "json")
            , ("count",  Just "20")
            , ("hoogle", Just (Text.encodeUtf8 q))
            ])

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
      let cleanItem = decodeEntities (stripTags (rhItm r))
          cleanTyp  = decodeEntities (stripTags (rhTyp r))
          (name, sigFromItem) = splitNameSig cleanItem
          sig = if Text.null cleanTyp then sigFromItem else cleanTyp
      in HoogleHit (rhPkg r) (rhMod r) name sig (decodeEntities (rhDoc r))

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
  -- Narrow exception scope: only catch 'HttpException' from
  -- 'http-client'.  parseRequest and httpLbs are the only operations
  -- inside this block, and both throw 'HttpException' in IO; any
  -- other (genuinely-unexpected) exception bubbles up to the
  -- top-level catchAny in app/hypha/Main.hs.
  reqE <- try (parseRequest (Text.unpack url))
            :: IO (Either HttpException Request)
  case reqE of
    Left e -> pure (Left (RemoteHttp (Text.pack (displayException e))))
    Right req0 -> do
      let req = req0 { responseTimeout =
                         responseTimeoutMicro (roTimeoutMicros opts) }
      r <- try (httpLbs req mgr)
             :: IO (Either HttpException (Response ByteString))
      case r of
        Left  e    -> pure (Left (RemoteHttp (Text.pack (displayException e))))
        Right resp -> pure (Right (responseBody resp))
