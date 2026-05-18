{-# LANGUAGE OverloadedStrings #-}
module Hypha.Server.Ui.Layout
  ( shellPage
  , breadcrumbs
  , helpOverlay
  ) where

import Data.List (intersperse)
import Data.Text (Text)
import Lucid

import qualified Hypha.Server.Ui.Search as UISearch
import qualified Hypha.Server.Ui.Tree   as UITree

-- | The shell HTML wrapping every view. Top search bar, sidebar tree,
-- breadcrumbs, and the main pane body.
shellPage :: Text                -- ^ page title
          -> [(Text, Text)]      -- ^ breadcrumbs (label, href)
          -> [Text]              -- ^ package list for sidebar tree
          -> Html ()             -- ^ main pane body
          -> Html ()
shellPage title crumbs pkgs body = doctypehtml_ $ do
  head_ $ do
    meta_ [charset_ "utf-8"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    title_ (toHtml title)
    link_ [rel_ "stylesheet", href_ "/assets/style.css"]
    script_ [src_ "/assets/htmx.min.js", defer_ ""] (mempty :: Text)
    script_ [src_ "/assets/keybindings.js", defer_ ""] (mempty :: Text)
  body_ $ do
    div_ [class_ "app"] $ do
      div_ [class_ "topbar"] $ do
        a_ [href_ "/", class_ "brand"] "hypha"
        UISearch.searchInput
      div_ [class_ "sidebar"] $ do
        div_ [class_ "section-title"] "Packages"
        UITree.packageTree pkgs
      div_ [class_ "main"] $ do
        breadcrumbs crumbs
        ul_ [class_ "results", id_ "results"] (pure ())
        body
    helpOverlay

-- | Help overlay listing every keybinding.  Toggled by the @?@ key
-- (see "ui/js/keybindings.js").  Hidden by default via CSS.
helpOverlay :: Html ()
helpOverlay = div_ [class_ "help-overlay"] $
  div_ [class_ "help-card"] $ do
    h2_ "Keyboard shortcuts"
    table_ $ do
      row "?"                "Toggle this help"
      row "/ , s, Ctrl-K"    "Focus search"
      row "j , \x2193"       "Next result"
      row "k , \x2191"       "Previous result"
      row "Enter"            "Open focused result"
      row "h , \x2190"       "History back"
      row "l , \x2192"       "History forward"
      row "g p"              "Go to packages"
      row "g h"              "Go home"
      row "Esc"              "Close / blur input"
    p_ [class_ "hint"] "Press ? again or Esc to close."
  where
    row :: Text -> Text -> Html ()
    row k v = tr_ $ do
      td_ [class_ "key"] (toHtml k)
      td_                (toHtml v)

-- | Render a breadcrumb trail. Each entry is a (label, href) pair.
-- Separators are placed /between/ items, never after the last one.
breadcrumbs :: [(Text, Text)] -> Html ()
breadcrumbs [] = mempty
breadcrumbs items = div_ [class_ "crumbs"] $
  mconcat (intersperse (" / " :: Html ()) (map breadcrumbItem items))
  where
    breadcrumbItem :: (Text, Text) -> Html ()
    breadcrumbItem (label, href) = a_ [href_ href] (toHtml label)
