{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Hypha.Server.Api
  ( HyphaApi
  , api
  , HTML
  , CSS
  , JS
  ) where

import qualified Data.ByteString.Lazy as BL
import Data.Proxy (Proxy (..))
import Lucid (Html, renderBS)
import Network.HTTP.Media ((//), (/:))
import Servant.API

-- | Custom HTML content type for servant, backed by 'lucid2'.
data HTML

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML (Html ()) where
  mimeRender _ = renderBS

-- | Custom @text/css@ content type for stylesheets.
data CSS
instance Accept CSS where
  contentType _ = "text" // "css" /: ("charset", "utf-8")
instance MimeRender CSS BL.ByteString where
  mimeRender _ = id

-- | Custom @application/javascript@ content type.
data JS
instance Accept JS where
  contentType _ = "application" // "javascript" /: ("charset", "utf-8")
instance MimeRender JS BL.ByteString where
  mimeRender _ = id

-- | All server routes.
--
-- @
-- GET  /                           → home
-- GET  /search?q=...               → search fragment
-- GET  /pkg/:pkg                   → package overview
-- GET  /pkg/:pkg/:mod              → module view
-- GET  /pkg/:pkg/:mod/:sym         → symbol card
-- GET  /haddock/:pkgver/:path      → rewritten Haddock HTML
-- GET  /source/:pkg/:mod           → source view
-- GET  /assets/style.css           → embedded CSS
-- GET  /assets/htmx.min.js         → embedded HTMX
-- GET  /assets/keybindings.js      → embedded keybindings
-- GET  /healthz                    → health check
-- @
type HyphaApi
  =    Get '[HTML] (Html ())
  :<|> "search"  :> QueryParam "q" String :> Get '[HTML] (Html ())
  :<|> "pkg"     :> Capture "pkg" String :> Get '[HTML] (Html ())
  :<|> "pkg"     :> Capture "pkg" String :> Capture "mod" String :> Get '[HTML] (Html ())
  :<|> "pkg"     :> Capture "pkg" String :> Capture "mod" String :> Capture "sym" String :> Get '[HTML] (Html ())
  :<|> "haddock" :> Capture "pkgver" String :> CaptureAll "path" String :> Get '[HTML] (Html ())
  :<|> "source"  :> Capture "pkg" String :> Capture "mod" String :> QueryParam "line" Int :> Get '[HTML] (Html ())
  :<|> "assets" :> "style.css"      :> Get '[CSS] BL.ByteString
  :<|> "assets" :> "htmx.min.js"    :> Get '[JS]  BL.ByteString
  :<|> "assets" :> "keybindings.js" :> Get '[JS]  BL.ByteString
  :<|> "healthz" :> Get '[PlainText] String

api :: Proxy HyphaApi
api = Proxy
