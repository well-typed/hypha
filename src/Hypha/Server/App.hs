{-# LANGUAGE OverloadedStrings #-}
module Hypha.Server.App
  ( appWith
  , ServerConfig (..)
    -- * Pure helpers (exported for tests)
  , sanitizeSegments
  , mimeFor
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as Text
import Data.Text (Text)
import Lucid
import Network.HTTP.Types (status200, status404)
import Network.Wai (Middleware, mapResponseHeaders, pathInfo, responseLBS)
import Servant

import qualified Hypha.Server.Assets   as Assets
import qualified Hypha.Server.Ui.Layout  as UI
import qualified Hypha.Server.Ui.ModuleDoc as UIMod
import qualified Hypha.Server.Ui.Search  as UISearch
import qualified Hypha.Server.Ui.Doc     as UIDoc
import qualified Hypha.Server.Ui.Source  as UISrc
import qualified Hypha.Server.Ui.Tree    as UITree
import Hypha.Types.PackageId (PackageName (..), Version (..))
import Hypha.Types.Route qualified as Route
import qualified Hypha.Search.Collapse   as Collapse
import qualified Hypha.Search.Fuzzy      as Fuzzy
import           Hypha.Server.Api       (HyphaApi, api)
import           Hypha.Server.ModuleDoc (ModuleDocView, SymbolCardData (..))
import           Hypha.Server.Slots     (BuildSlots)
import           Hypha.Types.BuildPlan  (PackageOrigin)

-- | Runtime configuration for the server, connecting the WAI app to
-- the application's data sources.
data ServerConfig = ServerConfig
  { scProjectName  :: !Text
  , scPackages     :: ![(Text, PackageOrigin)]
      -- ^ Sidebar entries.  The 'Text' is the human-facing component
      -- label (@pkg@, @pkg:sublib@, @pkg:exe:name@); the
      -- 'PackageOrigin' drives the per-entry provenance badge.
  , scSlots        :: !BuildSlots
  , scIndexReady   :: !(IO Bool)
      -- ^ Whether the in-memory search index has finished populating.
      -- Lets the search handler show a "Building the docs live\x2026"
      -- placeholder while the background indexer is still running.
  , scIndexProgress :: !(IO (Int, Int))
      -- ^ @(indexed, total)@ snapshot of the background indexer.  Drives
      -- the topbar progress bar.  Both are @0@ when nothing needed
      -- building (warm cache hit on every package).
  , scHumanSearch  :: !(Text -> Maybe Text -> IO [Collapse.SearchResult])
      -- ^ Query string plus an optional component to restrict to, returning
      -- the ranked, collapsed results.
      --
      -- The scope is the search's business rather than the caller's because
      -- it has to be applied to rows /before/ they are collapsed: a
      -- definition several packages present folds into one result carrying
      -- one component, so a caller filtering the results cannot see the
      -- other packages it belongs to.
  , scSymbolLookup :: !(Text -> Text -> Text -> IO (Maybe SymbolCardData))
      -- ^ pkg → mod → sym → everything the symbol card renders.
  , scHaddockFile  :: !(Text -> [Text] -> IO (Maybe (FilePath, BL.ByteString)))
      -- ^ \"\<pkg\>-\<ver\>\" + sanitised path segments → resolved file
      -- path (for MIME) and bytes; @.html@ payloads arrive already
      -- rewritten for the @/haddock/@ route.
  , scSourceText   :: !(Text -> Text -> IO (Maybe Text))
  , scPackageInfo  :: !(Text -> IO (Maybe (Text, [Text], PackageOrigin)))
      -- ^ Package overview: pkg → (version, top-level modules, origin).
      -- The origin is surfaced as the full chip on the package page so
      -- the user can confirm which copy of @pkg-ver@ they are looking
      -- at when multiple projects share a version.
  , scModuleDoc    :: !(Text -> Text -> IO ModuleDocView)
      -- ^ Module documentation, best available: prebuilt Haddock →
      -- source-rendered → export names with the degradation reason.
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
  :<|> progressPage cfg
  :<|> pkgPage cfg
  :<|> modPage cfg
  :<|> symPage cfg
  :<|> haddockApp cfg
  :<|> sourcePage cfg
  :<|> pure (BL.fromStrict Assets.cssBundle)
  :<|> pure (BL.fromStrict Assets.htmxJs)
  :<|> pure (BL.fromStrict Assets.keybindingsJs)
  :<|> pure (BL.fromStrict Assets.themeJs)
  :<|> pure "ok"

-- | Home page — landing with project headline + prominent search.
homePage :: ServerConfig -> Handler (Html ())
homePage cfg = pure $ UI.shellPage (scProjectName cfg) [] (scPackages cfg) $
  section_ [class_ "hero"] $ do
    h1_ (toHtml (scProjectName cfg))
    p_  [class_ "lede"]
      (toHtml ("Project-scoped Haskell documentation, straight from your build plan." :: Text))
    div_ [class_ "stat-row"] $ do
      statCard (length proj) "project components"
      statCard (length deps) "dependencies"
      statCard (length (scPackages cfg)) "total in plan"
  where
    (proj, deps) = UITree.splitByOrigin (scPackages cfg)

    statCard :: Int -> Text -> Html ()
    statCard n label = div_ [class_ "stat-card"] $ do
      div_ [class_ "stat-n"] (toHtml (Text.pack (show n)))
      div_ [class_ "stat-label"] (toHtml label)

-- | Progress fragment polled by the topbar progress bar (HTMX target).
-- The fragment carries its own @hx-trigger="every 1s"@ as long as the
-- indexer is still working, and stops polling once we're done.
progressPage :: ServerConfig -> Handler (Html ())
progressPage cfg = do
  ready          <- liftIO (scIndexReady cfg)
  (done, total)  <- liftIO (scIndexProgress cfg)
  pure (UI.progressFragment ready done total)

-- | Search results fragment (HTMX target).
searchPage :: ServerConfig -> Maybe String -> Maybe String -> Handler (Html ())
searchPage cfg mq mpkg = do
  let q = Text.strip (maybe "" Text.pack mq)
  if Text.null q
    then pure UISearch.emptyResults
    else do
      ready <- liftIO (scIndexReady cfg)
      if not ready
        then pure UISearch.buildingFragment
        else do
          rows <- liftIO (scHumanSearch cfg q (Text.pack <$> mpkg))
          pure (UISearch.resultsFragment (Fuzzy.tokenize q) rows)

-- | Package overview page — show pinned version + linked module index.
pkgPage :: ServerConfig -> String -> Handler (Html ())
pkgPage cfg pkg = do
  let pkgT = Text.pack pkg
  m <- liftIO (scPackageInfo cfg pkgT)
  let crumbs = [(pkgT, Route.hrefFrom ["pkg", pkgT])]
  pure $ UI.shellPage pkgT crumbs (scPackages cfg) $ case m of
    Nothing -> p_ [class_ "warn"] (toHtml ("Package " <> pkgT <> " not found."))
    Just (ver, mods, origin) -> div_ [class_ "pkg"] $ do
      div_ [class_ "pkg-head"] $ do
        h1_ (toHtml pkgT)
        p_  [class_ "meta"] $ do
          toHtml ("version " :: Text)
          code_ (toHtml ver)
        UITree.hackageLink (PackageName pkgT) (Version ver) origin
      UITree.originBadgeFull origin
      h2_ "Modules"
      if null mods
        then p_ [class_ "hint"] (toHtml ("No modules exposed." :: Text))
        else ul_ [class_ "module-list"] $
          mapM_ (\mp -> li_ $ a_ [href_ (Route.hrefFrom ["pkg", pkgT, mp])] (toHtml mp)) mods

-- | Module documentation view: prebuilt Haddock when available,
-- source-rendered docs otherwise, bare exports as the last resort.
modPage :: ServerConfig -> String -> String -> Handler (Html ())
modPage cfg pkg modPath = do
  let pkgT = Text.pack pkg
      modT = Text.pack modPath
  view <- liftIO (scModuleDoc cfg pkgT modT)
  let crumbs =
        [ (pkgT, Route.hrefFrom ["pkg", pkgT])
        , (modT, Route.hrefFrom ["pkg", pkgT, modT])
        ]
  pure $ UI.shellPage modT crumbs (scPackages cfg)
           (UIMod.modulePage pkgT modT view)

-- | Symbol documentation card.
symPage :: ServerConfig
        -> String -> String -> String
        -> Handler (Html ())
symPage cfg pkg modPath sym = do
  let pkgT = Text.pack pkg
      modT = Text.pack modPath
      symT = Text.pack sym
      crumbs =
        [ (pkgT, Route.hrefFrom ["pkg", pkgT])
        , (modT, Route.hrefFrom ["pkg", pkgT, modT])
        , (symT, Route.hrefFrom ["pkg", pkgT, modT, symT])
        ]
  m <- liftIO (scSymbolLookup cfg pkgT modT symT)
  case m of
    Nothing -> pure $ UI.shellPage symT crumbs (scPackages cfg) $
      p_ [class_ "warn"] "Symbol not found."
    Just card ->
      pure $ UI.shellPage symT crumbs (scPackages cfg)
                (UIDoc.symbolCard symT pkgT card)

-- | Serve raw Haddock files.  HTML pages arrive from 'scHaddockFile'
-- already link-rewritten; stylesheets, scripts, fonts, and images are
-- passed through with a MIME type inferred from their extension so the
-- original Haddock page renders properly.
haddockApp :: ServerConfig -> String -> Tagged Handler Application
haddockApp cfg pkgVer = Tagged $ \req send ->
  case sanitizeSegments (pathInfo req) of
    Nothing   -> send notFound
    Just segs -> do
      m <- scHaddockFile cfg (Text.pack pkgVer) segs
      case m of
        Nothing          -> send notFound
        Just (fp, bytes) ->
          send (responseLBS status200 [("Content-Type", mimeFor fp)] bytes)
  where
    notFound = responseLBS status404
      [("Content-Type", "text/plain; charset=utf-8")] "not found"

-- | Reject path traversal and other suspicious segments.  The server
-- only ever binds loopback, but serving @../../etc/passwd@ to
-- localhost is still a bug.
sanitizeSegments :: [Text] -> Maybe [Text]
sanitizeSegments segs
  | null segs               = Nothing
  | any suspicious segs     = Nothing
  | otherwise               = Just segs
  where
    suspicious s =
         Text.null s
      || s == ".." || s == "."
      || "." `Text.isPrefixOf` s
      || Text.any (\c -> c == '/' || c == '\\' || c == '\0') s

-- | MIME type from a file extension.  Haddock output only contains a
-- handful of asset types; anything unknown is served as opaque bytes.
mimeFor :: FilePath -> ByteString
mimeFor fp = case Text.toLower ext of
  "html"  -> "text/html; charset=utf-8"
  "css"   -> "text/css; charset=utf-8"
  "js"    -> "application/javascript; charset=utf-8"
  "json"  -> "application/json; charset=utf-8"
  "png"   -> "image/png"
  "gif"   -> "image/gif"
  "svg"   -> "image/svg+xml"
  "woff"  -> "font/woff"
  "woff2" -> "font/woff2"
  "txt"   -> "text/plain; charset=utf-8"
  _       -> "application/octet-stream"
  where
    ext = snd (Text.breakOnEnd "." (Text.pack fp))

-- | Source code view with skylighting-rendered Haskell + optional
-- @?line=N@ scroll target.
sourcePage :: ServerConfig -> String -> String -> Maybe Int -> Handler (Html ())
sourcePage cfg pkg modPath mLine = do
  let pkgT = Text.pack pkg
      modT = Text.pack modPath
      crumbs =
        [ (pkgT, Route.hrefFrom ["pkg", pkgT])
        , (modT, Route.hrefFrom ["pkg", pkgT, modT])
        , ("source", Route.hrefFrom ["source", pkgT, modT])
        ]
  m <- liftIO (scSourceText cfg pkgT modT)
  case m of
    Nothing -> pure $ UI.shellPage modT crumbs (scPackages cfg) $
      p_ [class_ "warn"] "Source not available for this module."
    Just t  -> pure $ UI.shellPage modT crumbs (scPackages cfg)
                       (UISrc.sourceView pkgT modT mLine t)
