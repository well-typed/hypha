{-# LANGUAGE OverloadedStrings #-}
module Hypha.Server.Ui.Layout
  ( shellPage
  , breadcrumbs
  ) where

import Data.List (intersperse)
import Data.Text (Text)
import Lucid

import qualified Hypha.Server.Ui.Search as UISearch
import qualified Hypha.Server.Ui.Tree   as UITree

-- | Shell HTML wrapping every view: sticky search bar, sidebar tree,
-- breadcrumbs, and the main pane body. The search bar lives at the top of
-- every page, so dedicated focus keybindings would only add noise.
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
        body

-- | Render a breadcrumb trail. Each entry is a (label, href) pair.
-- Separators are placed /between/ items, never after the last one.
breadcrumbs :: [(Text, Text)] -> Html ()
breadcrumbs [] = mempty
breadcrumbs items = div_ [class_ "crumbs"] $
  mconcat (intersperse (" / " :: Html ()) (map breadcrumbItem items))
  where
    breadcrumbItem :: (Text, Text) -> Html ()
    breadcrumbItem (label, href) = a_ [href_ href] (toHtml label)
