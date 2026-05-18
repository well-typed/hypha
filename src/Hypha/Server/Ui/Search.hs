{-# LANGUAGE OverloadedStrings #-}
module Hypha.Server.Ui.Search
  ( searchInput
  , resultsFragment
  ) where

import Data.Text (Text)
import Lucid
import Lucid.Base (makeAttributes)

-- | The search input bar with HTMX live-search attributes.
searchInput :: Html ()
searchInput = input_
  [ class_       "search-input"
  , type_        "search"
  , name_        "q"
  , placeholder_ "search (press s, /, or Ctrl-K)"
  , autocomplete_ "off"
  , autofocus_
  , makeAttributes "hx-get"     "/search"
  , makeAttributes "hx-trigger" "keyup changed delay:120ms"
  , makeAttributes "hx-target"  "#results"
  ]

-- | Render search results as an unordered list.
-- Each row carries (package, module path, symbol name, signature).
resultsFragment :: [(Text, Text, Text, Text)] -> Html ()
resultsFragment rows = ul_ [class_ "results", id_ "results"] $ mapM_ row rows
  where
    row :: (Text, Text, Text, Text) -> Html ()
    row (pkg, modPath, name, sig) = li_ $ do
      a_ [href_ ("/pkg/" <> pkg <> "/" <> modPath <> "/" <> name)] $ do
        span_ [class_ "name"]   (toHtml name)
        span_ [class_ "sig"]    (toHtml sig)
        span_ [class_ "pkgmod"] (toHtml (pkg <> " \183 " <> modPath))
