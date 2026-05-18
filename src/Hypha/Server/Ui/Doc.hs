{-# LANGUAGE OverloadedStrings #-}
module Hypha.Server.Ui.Doc
  ( symbolCard
  ) where

import Data.Text (Text)
import Lucid

-- | Render a symbol documentation card with name, signature, rendered Haddock
-- prose, and a link to the source file.
symbolCard :: Text  -- ^ symbol name
           -> Text  -- ^ type signature
           -> Text  -- ^ rendered Haddock HTML
           -> Text  -- ^ source file path
           -> Int   -- ^ source line number
           -> Html ()
symbolCard name sig haddockHtml srcPath srcLine = div_ [class_ "doc"] $ do
  h2_ [class_ "symbol-name"] (toHtml name)
  pre_ [class_ "signature"] (code_ (toHtml sig))
  div_ [class_ "haddock"] (toHtmlRaw haddockHtml)
  div_ [class_ "src-link"] $ do
    toHtml ("source: " :: Text)
    a_ [href_ ("/source/" <> srcPath)] (toHtml srcPath)
    toHtml (":" :: Text)
    toHtml (show srcLine)
