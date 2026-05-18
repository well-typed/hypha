{-# LANGUAGE OverloadedStrings #-}
module Hypha.Server.Ui.Tree
  ( packageTree
  ) where

import Data.Text (Text)
import Lucid

-- | Sidebar package list linking to each package's overview page.
packageTree :: [Text] -> Html ()
packageTree pkgs = ul_ [class_ "tree"] $
  mapM_ (\p -> li_ $ a_ [href_ ("/pkg/" <> p)] (toHtml p)) pkgs
