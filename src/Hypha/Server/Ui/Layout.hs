{-# LANGUAGE OverloadedStrings #-}
module Hypha.Server.Ui.Layout
  ( shellPage
  , breadcrumbs
  , progressFragment
  ) where

import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as Text
import Lucid
import Lucid.Base (makeAttributes)

import qualified Hypha.Server.Ui.Search as UISearch
import qualified Hypha.Server.Ui.Tree   as UITree
import Hypha.Types.BuildPlan (PackageOrigin)

-- | Shell HTML wrapping every view: sticky search bar, sidebar tree,
-- breadcrumbs, and the main pane body.  Carries a thin progress strip
-- pinned to the very top of the page that polls @\/progress@ until the
-- background indexer reports done.
shellPage :: Text                          -- ^ page title
          -> [(Text, Text)]                -- ^ breadcrumbs (label, href)
          -> [(Text, PackageOrigin)]       -- ^ package list for sidebar tree
          -> Html ()                       -- ^ main pane body
          -> Html ()
shellPage title crumbs pkgs body = doctypehtml_ $ do
  head_ $ do
    meta_ [charset_ "utf-8"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    title_ (toHtml title)
    link_ [rel_ "stylesheet", href_ "/assets/style.css"]
    -- theme.js runs synchronously so data-theme lands before first
    -- paint — no flash of the wrong theme.  CSP forbids inlining it.
    script_ [src_ "/assets/theme.js"] (mempty :: Text)
    script_ [src_ "/assets/htmx.min.js", defer_ ""] (mempty :: Text)
    script_ [src_ "/assets/keybindings.js", defer_ ""] (mempty :: Text)
  body_ $ do
    -- Initial progress slot — htmx replaces this with a real fragment
    -- on first poll.  Rendered hidden so the page never flashes a
    -- "Building" strip on an already-warm cache; the first /progress
    -- response will show it again if the indexer is still working.
    div_ [ id_ "progress-host"
         , makeAttributes "hx-get"     "/progress"
         , makeAttributes "hx-trigger" "load"
         , makeAttributes "hx-swap"    "outerHTML"
         ] (pure ())
    div_ [class_ "app"] $ do
      div_ [class_ "topbar"] $ do
        a_ [href_ "/", class_ "brand"] "hypha"
        UISearch.searchInput
        button_ [ id_ "theme-toggle"
                , class_ "theme-toggle"
                , type_ "button"
                , title_ "Theme"
                ]
                "Auto"
      div_ [class_ "sidebar"] $ UITree.sidebar pkgs
      div_ [class_ "main"] $ do
        breadcrumbs crumbs
        body

-- | Topbar progress bar fragment.  Self-polls while the indexer is
-- still working; emits a final "done" state once that holds true so
-- htmx stops issuing GET /progress.
progressFragment
  :: Bool   -- ^ indexer ready?
  -> Int    -- ^ packages indexed so far
  -> Int    -- ^ total packages to index (0 means no work was queued)
  -> Html ()
progressFragment ready done total
  | ready || total == 0 =
      -- Stay in the DOM so htmx can re-poll if the page survives a long
      -- session; but mark @.done@ so the CSS animates a fade-out.
      div_ [ id_ "progress-host"
           , class_ "progress-host done"
           ] (pure ())
  | otherwise =
      let pct :: Int
          pct = max 0 (min 100 (done * 100 `div` max 1 total))
          label = Text.pack (show done <> "/" <> show total <> " packages")
      in div_ [ id_ "progress-host"
              , class_ "progress-host live"
              , makeAttributes "hx-get"     "/progress"
              , makeAttributes "hx-trigger" "every 800ms"
              , makeAttributes "hx-swap"    "outerHTML"
              ] $ do
        div_ [class_ "progress-bar"] $
          div_ [ class_ "progress-bar-fill"
               , style_ ("width:" <> Text.pack (show pct) <> "%")
               ] (pure ())
        div_ [class_ "progress-label"] $ do
          strong_ "Indexing\x2026 "
          span_ [class_ "progress-count"] (toHtml label)
          span_ [class_ "progress-pct"]
            (toHtml (" \x00B7 " <> Text.pack (show pct) <> "%"))

-- | Render a breadcrumb trail. Each entry is a (label, href) pair.
-- Separators are placed /between/ items, never after the last one.
breadcrumbs :: [(Text, Text)] -> Html ()
breadcrumbs [] = mempty
breadcrumbs items = div_ [class_ "crumbs"] $
  mconcat (intersperse (" / " :: Html ()) (map breadcrumbItem items))
  where
    breadcrumbItem :: (Text, Text) -> Html ()
    breadcrumbItem (label, href) = a_ [href_ href] (toHtml label)
