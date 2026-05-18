{-# LANGUAGE OverloadedStrings #-}
module Hypha.Server.App
  ( appWith
  , cspMiddleware
  , ServerConfig (..)
  ) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Lucid
import Network.Wai (Middleware, mapResponseHeaders)
import Servant

import qualified Hypha.Server.Assets   as Assets
import qualified Hypha.Server.Ui.Layout  as UI
import qualified Hypha.Server.Ui.Search  as UISearch
import qualified Hypha.Server.Ui.Doc     as UIDoc
import qualified Hypha.Server.Ui.Source  as UISrc
import           Hypha.Server.Api       (HyphaApi, api)
import           Hypha.Server.Slots     (BuildSlots)

-- | Runtime configuration for the server, connecting the WAI app to
-- the application's data sources.
data ServerConfig = ServerConfig
  { scProjectName  :: !Text
  , scPackages     :: ![Text]
  , scSlots        :: !BuildSlots
  , scHumanSearch  :: !(Text -> IO [(Text, Text, Text, Text)])
      -- ^ Given a query string, return (package, module, name, signature)
  , scSymbolLookup :: !(Text -> Text -> Text -> IO (Maybe (Text, Text, Text, Int)))
      -- ^ pkg → mod → sym → (signature, haddockHtml, srcPath, srcLine)
  , scHaddockHtml  :: !(Text -> [String] -> IO (Maybe BL.ByteString))
      -- ^ \"\<pkg\>-\<ver\>\" + path segments → raw bytes (already rewritten)
  , scSourceText   :: !(Text -> Text -> IO (Maybe Text))
  }

-- | Build a WAI 'Application' from the given 'ServerConfig'.  The CSP
-- middleware is layered on by default — strict allowlist appropriate for
-- a local, self-served browser.
appWith :: ServerConfig -> Application
appWith cfg = cspMiddleware (serve api (server cfg))

-- | Inject a strict Content-Security-Policy header on every response.
-- Permits inline styles (htmx targets need them) but forbids inline scripts
-- and remote origins.
cspMiddleware :: Middleware
cspMiddleware = \app req send ->
  app req $ \resp ->
    send (mapResponseHeaders (("Content-Security-Policy", cspValue) :) resp)
  where
    cspValue =
      "default-src 'self'; img-src 'self' data:; style-src 'self' 'unsafe-inline'"

server :: ServerConfig -> Server HyphaApi
server cfg =
       homePage cfg
  :<|> searchPage cfg
  :<|> pkgPage cfg
  :<|> modPage cfg
  :<|> symPage cfg
  :<|> haddockPage cfg
  :<|> sourcePage cfg
  :<|> pure (BL.fromStrict Assets.cssBundle)
  :<|> pure (BL.fromStrict Assets.htmxJs)
  :<|> pure (BL.fromStrict Assets.keybindingsJs)
  :<|> pure "ok"

-- | Home page — landing with search bar.
homePage :: ServerConfig -> Handler (Html ())
homePage cfg = pure $ UI.shellPage (scProjectName cfg) [] (scPackages cfg) $
  p_ $ do
    toHtml ("Welcome to hypha. Press " :: Text)
    code_ "s"
    toHtml (" to search." :: Text)

-- | Search results fragment (HTMX target).
searchPage :: ServerConfig -> Maybe String -> Handler (Html ())
searchPage cfg mq = do
  let q = maybe "" Text.pack mq
  rows <- liftIO (scHumanSearch cfg q)
  pure (UISearch.resultsFragment rows)

-- | Package overview page.
pkgPage :: ServerConfig -> String -> Handler (Html ())
pkgPage cfg pkg = pure $ UI.shellPage (Text.pack pkg) [] (scPackages cfg) $
  p_ (toHtml ("Package " <> Text.pack pkg))

-- | Module view page.
modPage :: ServerConfig -> String -> String -> Handler (Html ())
modPage cfg pkg modPath = pure $
  UI.shellPage (Text.pack modPath) [] (scPackages cfg) $
    p_ (toHtml ("Module " <> Text.pack modPath <> " in " <> Text.pack pkg))

-- | Symbol documentation card.
symPage :: ServerConfig
        -> String -> String -> String
        -> Handler (Html ())
symPage cfg pkg modPath sym = do
  m <- liftIO (scSymbolLookup cfg (Text.pack pkg) (Text.pack modPath) (Text.pack sym))
  case m of
    Nothing -> pure $ UI.shellPage (Text.pack sym) [] (scPackages cfg) $
      p_ "not found"
    Just (sig, hd, srcPath, srcLine) ->
      pure $ UI.shellPage (Text.pack sym) [] (scPackages cfg)
                (UIDoc.symbolCard (Text.pack sym) sig hd srcPath srcLine)

-- | Serve rewritten Haddock HTML.
haddockPage :: ServerConfig -> String -> [String] -> Handler (Html ())
haddockPage cfg pkgVer path = do
  m <- liftIO (scHaddockHtml cfg (Text.pack pkgVer) path)
  case m of
    Nothing -> pure (p_ "not found")
    Just bs -> pure (toHtmlRaw (Text.decodeUtf8 (BL.toStrict bs)))

-- | Source code view.
sourcePage :: ServerConfig -> String -> String -> Handler (Html ())
sourcePage cfg pkg modPath = do
  m <- liftIO (scSourceText cfg (Text.pack pkg) (Text.pack modPath))
  case m of
    Nothing -> pure (p_ "not found")
    Just t  -> pure (UI.shellPage (Text.pack modPath) [] (scPackages cfg)
                       (UISrc.sourceView t))
