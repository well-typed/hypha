{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
-- | Typed @--bind@ address handling for @hypha server@.
--
-- Pulled out of "Hypha.Command.Server" into its own leaf module so the
-- 'BindError' value can be embedded structurally in
-- 'Hypha.Error.HyphaError' without forcing a cycle through the heavy
-- server module.
module Hypha.Server.Bind
  ( BindAddr (..)
  , BindError (..)
  , renderBindError
  , parseBind
  , defaultBindAddr
  , portFromInt
  , renderBindUrl
  ) where

import Data.IP
  ( AddrRange, IP (..), IPv4, IPv6, isMatchedTo, makeAddrRange, toIPv4, toIPv6 )
import Data.Text (Text)
import qualified Data.Text as Text
import Network.Socket (PortNumber)
import Network.URI ( URI (..), URIAuth (..), parseURIReference, uriToString )
import Text.Read (readMaybe)

-- | Bind address — a typed IP plus a bounded port.  Loopback membership
-- enforced at construction time by 'parseBind' / 'defaultBindAddr'.
data BindAddr = BindAddr
  { baIP   :: !IP
  , baPort :: !PortNumber
  } deriving stock (Show, Eq)

-- | Parse failure or refusal.
data BindError
  = BindMalformed   !Text  -- ^ Could not parse @HOST:PORT@.
  | BindNonLoopback !Text  -- ^ Caller asked for a non-loopback bind.
  deriving stock (Show, Eq)

-- | User-facing renderer for 'BindError'.  Only call this at the wire
-- boundary (envelope message, stderr) — never inside an error
-- constructor.
renderBindError :: BindError -> Text
renderBindError = \case
  BindMalformed   raw -> "malformed --bind value: " <> raw
  BindNonLoopback raw -> "refusing non-loopback bind: " <> raw

-- | IPv4 loopback range: @127.0.0.0/8@.
ipv4Loopback :: AddrRange IPv4
ipv4Loopback = makeAddrRange (toIPv4 [127,0,0,0]) 8

-- | IPv6 loopback range: @::1/128@.
ipv6Loopback :: AddrRange IPv6
ipv6Loopback = makeAddrRange (toIPv6 [0,0,0,0,0,0,0,1]) 128

-- | Is this IP inside the standard loopback ranges?
isLoopbackIP :: IP -> Bool
isLoopbackIP = \case
  IPv4 a -> a `isMatchedTo` ipv4Loopback
  IPv6 a -> a `isMatchedTo` ipv6Loopback

-- | Default IPv4 loopback bind: @127.0.0.1:<port>@.
defaultBindAddr :: PortNumber -> BindAddr
defaultBindAddr = BindAddr (IPv4 (toIPv4 [127,0,0,1]))

-- | Convert a raw 'Int' (from the CLI) to a bounded 'PortNumber'.
portFromInt :: Int -> Maybe PortNumber
portFromInt n
  | n >= 1 && n <= 65535 = Just (fromIntegral n)
  | otherwise            = Nothing

-- | Render a 'BindAddr' as an @http://@ URL.  IPv6 addresses are wrapped
-- in square brackets per RFC 3986; rendering is delegated to
-- @network-uri@ to keep the encoding rules in one place.
renderBindUrl :: BindAddr -> String
renderBindUrl = ($ "") . uriToString id . bindAsURI

-- | Reflect a 'BindAddr' back into a 'URI' value so the @network-uri@
-- machinery handles bracket placement and serialisation.
bindAsURI :: BindAddr -> URI
bindAsURI (BindAddr ip port) = URI
  { uriScheme    = "http:"
  , uriAuthority = Just URIAuth
      { uriUserInfo = ""
      , uriRegName  = case ip of
          IPv4 a -> show a
          IPv6 a -> "[" <> show a <> "]"
      , uriPort     = ':' : show port
      }
  , uriPath      = ""
  , uriQuery     = ""
  , uriFragment  = ""
  }

-- | Parse a bind string.  Accepts:
--
--   * @127.0.0.1:4287@ — IPv4 with port;
--   * @localhost:4287@ — short-circuited to @127.0.0.1@ (no DNS);
--   * @[::1]:4287@     — IPv6 in RFC 3986 brackets.
--
-- A 'URI' authority is built eagerly via 'parseBindAuthority'; the
-- typed @host@ and @port@ are then derived from it.  Only loopback
-- ranges (@127.0.0.0/8@ and @::1/128@) are accepted; any routable
-- address returns 'BindNonLoopback'.
parseBind :: Text -> Either BindError BindAddr
parseBind raw = do
  auth <- noteMalformed (parseBindAuthority raw)
  ip   <- noteMalformed (parseBindHost (uriRegName auth))
  port <- noteMalformed (parseBindPort (uriPort auth))
  if isLoopbackIP ip
    then Right (BindAddr ip port)
    else Left (BindNonLoopback raw)
  where
    noteMalformed = maybe (Left (BindMalformed raw)) Right

-- | Parse the raw bind string as a strict RFC 3986 authority via
-- @network-uri@'s 'parseURIReference' (fed as the network-path
-- reference @"//<raw>"@).  Every URI component outside the
-- authority (scheme, path, query, fragment, userinfo) is rejected
-- so only pure @HOST:PORT@ shapes survive.
parseBindAuthority :: Text -> Maybe URIAuth
parseBindAuthority raw = do
  uri <- parseURIReference ("//" <> Text.unpack raw)
  guardEmpty (uriScheme uri)
  guardEmpty (uriPath uri)
  guardEmpty (uriQuery uri)
  guardEmpty (uriFragment uri)
  auth <- uriAuthority uri
  guardEmpty (uriUserInfo auth)
  pure auth
  where
    guardEmpty s = if null s then Just () else Nothing

-- | Parse the @host@ component of a 'URIAuth' as an 'IP'.  Strips
-- the IPv6 brackets that 'parseURIReference' keeps on @uriRegName@,
-- and short-circuits @localhost@ to IPv4 loopback (no DNS lookup).
parseBindHost :: String -> Maybe IP
parseBindHost "localhost" = Just (IPv4 (toIPv4 [127,0,0,1]))
parseBindHost regName     = readMaybe (stripBrackets regName)
  where
    stripBrackets ('[' : rest@(_:_)) | last rest == ']' = init rest
    stripBrackets s                                     = s

-- | Parse the @port@ component of a 'URIAuth' (which 'network-uri'
-- delivers including its leading @\':'@) as a bounded 'PortNumber'.
parseBindPort :: String -> Maybe PortNumber
parseBindPort (':' : s) | not (null s) = readMaybe s
parseBindPort _                        = Nothing
