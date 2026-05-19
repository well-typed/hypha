{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell    #-}
module Hypha.Server.Assets
  ( cssBundle
  , htmxJs
  , keybindingsJs
  , iconSearch
  , iconPackage
  , iconModule
  ) where

import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile)

cssBundle :: ByteString
cssBundle =
     $(embedFile "ui/css/base.css")
  <> "\n" <> $(embedFile "ui/css/layout.css")
  <> "\n" <> $(embedFile "ui/css/components/search.css")
  <> "\n" <> $(embedFile "ui/css/components/tree.css")
  <> "\n" <> $(embedFile "ui/css/components/doc.css")
  <> "\n" <> $(embedFile "ui/css/components/progress.css")

htmxJs, keybindingsJs :: ByteString
htmxJs        = $(embedFile "ui/js/htmx.min.js")
keybindingsJs = $(embedFile "ui/js/keybindings.js")

iconSearch, iconPackage, iconModule :: ByteString
iconSearch  = $(embedFile "ui/icons/search.svg")
iconPackage = $(embedFile "ui/icons/package.svg")
iconModule  = $(embedFile "ui/icons/module.svg")
