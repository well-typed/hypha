{-# LANGUAGE OverloadedStrings #-}
module Hypha.Server.Ui.Search
  ( searchInput
  , searchPanel
  , resultsFragment
  , buildingFragment
  ) where

import Data.Text (Text)
import Lucid
import Lucid.Base (makeAttributes)

-- | Search input + empty results container. HTMX swaps content into
-- @#results@ as the user types.
searchPanel :: Html ()
searchPanel = do
  searchInput
  ul_ [class_ "results", id_ "results"] (pure ())

-- | Search input bar with HTMX live-search attributes and an inline
-- progress spinner controlled by the @htmx-request@ class.
searchInput :: Html ()
searchInput = do
  div_ [class_ "search-wrap"] $ do
    span_ [class_ "search-indicator", title_ "Searching\x2026"] $ do
      span_ [class_ "spinner"]     (pure ())
      span_ [class_ "indicator-label"] "Searching\x2026"
    input_
      [ class_       "search-input"
      , type_        "search"
      , name_        "q"
      , placeholder_ "Search packages, modules, symbols\x2026"
      , autocomplete_ "off"
      , autofocus_
      , makeAttributes "hx-get"       "/search"
      , makeAttributes "hx-trigger"   "keyup changed delay:120ms"
      , makeAttributes "hx-target"    "#results"
      , makeAttributes "hx-indicator" ".search-indicator"
      ]

-- | Render search results as an unordered list.
-- Each row carries (package, module path, symbol name, signature).
resultsFragment :: [(Text, Text, Text, Text)] -> Html ()
resultsFragment rows = ul_ [class_ "results", id_ "results"] $
  if null rows
    then li_ [class_ "empty"] "No matches."
    else mapM_ row rows
  where
    row :: (Text, Text, Text, Text) -> Html ()
    row (pkg, modPath, name, sig) = li_ $ do
      a_ [href_ ("/pkg/" <> pkg <> "/" <> modPath <> "/" <> name)] $ do
        span_ [class_ "name"]   (toHtml name)
        span_ [class_ "sig"]    (toHtml sig)
        span_ [class_ "pkgmod"] (toHtml (pkg <> " \183 " <> modPath))

-- | Friendly placeholder shown while the in-memory index is still being
-- populated in the background.  Re-issues the request shortly after so the
-- UI catches up without user action.
buildingFragment :: Html ()
buildingFragment = ul_ [class_ "results", id_ "results"]
  $ li_ [ class_ "building"
        , makeAttributes "hx-get"     "/search"
        , makeAttributes "hx-trigger" "load delay:600ms"
        , makeAttributes "hx-target"  "#results"
        , makeAttributes "hx-include" ".search-input"
        ] $ do
      span_ [class_ "spinner big"] (pure ())
      div_  [class_ "building-text"] $ do
        strong_ "Building the docs live\x2026"
        span_ [class_ "building-sub"]
          "Indexing your build plan. Results appear as they come in."
