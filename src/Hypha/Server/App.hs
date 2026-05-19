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
  , scIndexReady   :: !(IO Bool)
      -- ^ Whether the in-memory search index has finished populating.
      -- Lets the search handler show a "Building the docs live\x2026"
      -- placeholder while the background indexer is still running.
  , scHumanSearch  :: !(Text -> IO [(Text, Text, Text, Text)])
      -- ^ Given a query string, return (package, module, name, signature)
  , scSymbolLookup :: !(Text -> Text -> Text -> IO (Maybe (Text, Text, Text, Maybe Int)))
      -- ^ pkg → mod → sym → (signature, haddockHtml, resolvedModule, srcLine).
      -- @srcLine@ is 'Nothing' when no faithful source line can be
      -- determined (instead of falling back to a bogus @:1@).
      -- @resolvedModule@ is the module that actually defines the symbol
      -- (re-exports collapse: @Data.Map.Strict.lookup@ → @Data.Map.Internal@).
  , scHaddockHtml  :: !(Text -> [String] -> IO (Maybe BL.ByteString))
      -- ^ \"\<pkg\>-\<ver\>\" + path segments → raw bytes (already rewritten)
  , scSourceText   :: !(Text -> Text -> IO (Maybe Text))
  , scPackageInfo  :: !(Text -> IO (Maybe (Text, [Text])))
      -- ^ Package overview: pkg → (version, top-level modules)
  , scModuleExports :: !(Text -> Text -> IO [Text])
      -- ^ Module export list: pkg → mod → [symbol names]
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

-- | Home page — landing with project headline + prominent search.
homePage :: ServerConfig -> Handler (Html ())
homePage cfg = pure $ UI.shellPage (scProjectName cfg) [] (scPackages cfg) $
  section_ [class_ "hero"] $ do
    h1_ (toHtml (scProjectName cfg))
    p_  [class_ "lede"] $ do
      toHtml ("Browsing " :: Text)
      strong_ (toHtml (Text.pack (show (length (scPackages cfg)))))
      toHtml (" packages from your build plan." :: Text)
    p_  [class_ "hint"]
      (toHtml ("Start typing in the search bar above to jump to a symbol, module, or package." :: Text))

-- | Search results fragment (HTMX target).
searchPage :: ServerConfig -> Maybe String -> Handler (Html ())
searchPage cfg mq = do
  let q = Text.strip (maybe "" Text.pack mq)
  if Text.null q
    then pure UISearch.emptyResults
    else do
      ready <- liftIO (scIndexReady cfg)
      if not ready
        then pure UISearch.buildingFragment
        else do
          rows <- liftIO (scHumanSearch cfg q)
          pure (UISearch.resultsFragment rows)

-- | Package overview page — show pinned version + linked module index.
pkgPage :: ServerConfig -> String -> Handler (Html ())
pkgPage cfg pkg = do
  let pkgT = Text.pack pkg
  m <- liftIO (scPackageInfo cfg pkgT)
  let crumbs = [(pkgT, "/pkg/" <> pkgT)]
  pure $ UI.shellPage pkgT crumbs (scPackages cfg) $ case m of
    Nothing -> p_ [class_ "warn"] (toHtml ("Package " <> pkgT <> " not found."))
    Just (ver, mods) -> div_ [class_ "pkg"] $ do
      h1_ (toHtml pkgT)
      p_  [class_ "meta"] $ do
        toHtml ("version " :: Text)
        code_ (toHtml ver)
      h2_ "Modules"
      if null mods
        then p_ [class_ "hint"] (toHtml ("No modules exposed." :: Text))
        else ul_ [class_ "module-list"] $
          mapM_ (\mp -> li_ $ a_ [href_ ("/pkg/" <> pkgT <> "/" <> mp)] (toHtml mp)) mods

-- | Module view page — list exports with links to symbol cards.
modPage :: ServerConfig -> String -> String -> Handler (Html ())
modPage cfg pkg modPath = do
  let pkgT = Text.pack pkg
      modT = Text.pack modPath
  exps <- liftIO (scModuleExports cfg pkgT modT)
  let crumbs =
        [ (pkgT, "/pkg/" <> pkgT)
        , (modT, "/pkg/" <> pkgT <> "/" <> modT)
        ]
  pure $ UI.shellPage modT crumbs (scPackages cfg) $ div_ [class_ "mod"] $ do
    h1_ (toHtml modT)
    p_  [class_ "meta"] $ do
      toHtml ("in package " :: Text)
      a_ [href_ ("/pkg/" <> pkgT)] (toHtml pkgT)
    h2_ "Exports"
    if null exps
      then p_ [class_ "hint"] (toHtml ("No exports detected." :: Text))
      else ul_ [class_ "export-list"] $
        mapM_ (\nm -> li_ $
                 a_ [href_ ("/pkg/" <> pkgT <> "/" <> modT <> "/" <> nm)]
                    (code_ (toHtml nm)))
              exps
    p_ [class_ "footer-actions"] $
      a_ [href_ ("/source/" <> pkgT <> "/" <> modT)] (toHtml ("View source" :: Text))

-- | Symbol documentation card.
symPage :: ServerConfig
        -> String -> String -> String
        -> Handler (Html ())
symPage cfg pkg modPath sym = do
  let pkgT = Text.pack pkg
      modT = Text.pack modPath
      symT = Text.pack sym
      crumbs =
        [ (pkgT, "/pkg/" <> pkgT)
        , (modT, "/pkg/" <> pkgT <> "/" <> modT)
        , (symT, "/pkg/" <> pkgT <> "/" <> modT <> "/" <> symT)
        ]
  m <- liftIO (scSymbolLookup cfg pkgT modT symT)
  case m of
    Nothing -> pure $ UI.shellPage symT crumbs (scPackages cfg) $
      p_ [class_ "warn"] "Symbol not found."
    Just (sig, hd, resolvedMod, mLine) ->
      pure $ UI.shellPage symT crumbs (scPackages cfg)
                (UIDoc.symbolCard symT sig hd pkgT resolvedMod mLine)

-- | Serve rewritten Haddock HTML.
haddockPage :: ServerConfig -> String -> [String] -> Handler (Html ())
haddockPage cfg pkgVer path = do
  m <- liftIO (scHaddockHtml cfg (Text.pack pkgVer) path)
  case m of
    Nothing -> pure (p_ "not found")
    Just bs -> pure (toHtmlRaw (Text.decodeUtf8 (BL.toStrict bs)))

-- | Source code view with skylighting-rendered Haskell + optional
-- @?line=N@ scroll target.
sourcePage :: ServerConfig -> String -> String -> Maybe Int -> Handler (Html ())
sourcePage cfg pkg modPath mLine = do
  let pkgT = Text.pack pkg
      modT = Text.pack modPath
      crumbs =
        [ (pkgT, "/pkg/" <> pkgT)
        , (modT, "/pkg/" <> pkgT <> "/" <> modT)
        , ("source", "/source/" <> pkgT <> "/" <> modT)
        ]
  m <- liftIO (scSourceText cfg pkgT modT)
  case m of
    Nothing -> pure $ UI.shellPage modT crumbs (scPackages cfg) $
      p_ [class_ "warn"] "Source not available for this module."
    Just t  -> pure $ UI.shellPage modT crumbs (scPackages cfg)
                       (UISrc.sourceView modT mLine t)
