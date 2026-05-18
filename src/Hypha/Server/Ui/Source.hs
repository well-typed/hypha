{-# LANGUAGE OverloadedStrings #-}
module Hypha.Server.Ui.Source
  ( sourceView
  ) where

import Data.Text (Text)
import Lucid

-- | Render source code inside a preformatted block.
sourceView :: Text -> Html ()
sourceView body = pre_ [class_ "source"] (code_ (toHtml body))
