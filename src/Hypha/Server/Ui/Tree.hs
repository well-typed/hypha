{-# LANGUAGE OverloadedStrings #-}
module Hypha.Server.Ui.Tree
  ( packageTree
  ) where

import Data.Text (Text)
import Lucid

-- | Sidebar package list linking to each package's overview page.
packageTree :: [Text] -> Html ()
packageTree = ul_ [class_ "tree"] . mconcat . map packageLink
  where
    packageLink :: Text -> Html ()
    packageLink p = li_ $ a_ [href_ ("/pkg/" <> p)] (toHtml p)
