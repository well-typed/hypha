{-# LANGUAGE OverloadedStrings #-}
module Hypha.Server.Ui.Doc
  ( symbolCard
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import Lucid

-- | Render a symbol documentation card with name, signature, rendered Haddock
-- prose, and a link to the source view rooted at the package/module pair.
-- The link omits the line anchor when no faithful source line could be
-- located, so we never produce a bogus @:1@ for files we couldn't parse.
symbolCard
  :: Text       -- ^ symbol name
  -> Text       -- ^ type signature
  -> Text       -- ^ rendered Haddock prose
  -> Text       -- ^ package name
  -> Text       -- ^ module path (dotted)
  -> Maybe Int  -- ^ source line, when known
  -> Html ()
symbolCard name sig haddockText pkg modPath mLine = div_ [class_ "doc"] $ do
  h2_ [class_ "symbol-name"] (toHtml name)
  pre_ [class_ "signature"] (code_ (toHtml sig))
  div_ [class_ "haddock"] (toHtml haddockText)
  div_ [class_ "src-link"] $ do
    toHtml ("source: " :: Text)
    case mLine of
      Just srcLine ->
        a_ [ href_ ("/source/" <> pkg <> "/" <> modPath
                     <> "?line=" <> Text.pack (show srcLine)
                     <> "#L"     <> Text.pack (show srcLine))
           ]
           (toHtml (pkg <> "/" <> modPath <> ":" <> Text.pack (show srcLine)))
      Nothing ->
        a_ [ href_ ("/source/" <> pkg <> "/" <> modPath) ]
           (toHtml (pkg <> "/" <> modPath))
