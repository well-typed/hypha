{-# LANGUAGE OverloadedStrings #-}
module Hypha.Server.Ui.Doc
  ( symbolCard
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import Lucid
import Lucid.Base (makeAttributes)

import qualified Hypha.Server.Ui.Haddock as Haddock
import           Hypha.Server.ModuleDoc (SymbolCardData (..))
import           Hypha.Server.Ui.ModuleDoc (anchorFor, kindBadge)
import           Hypha.Source.Parser (DeclKind (..))

-- | Render a symbol documentation card: kind badge, name, copyable
-- signature, rendered Haddock prose, and links back to the module doc
-- and the source view.  The source link omits the line anchor when no
-- faithful source line could be located, so we never produce a bogus
-- @:1@ for files we couldn't parse.
symbolCard
  :: Text            -- ^ symbol name
  -> Text            -- ^ package name (URL component)
  -> SymbolCardData
  -> Html ()
symbolCard name pkg card = div_ [class_ "doc"] $ do
  div_ [class_ "symbol-head"] $ do
    kindBadge kind
    h2_ [class_ "symbol-name"] (toHtml name)
    button_ [ class_ "copy-btn"
            , type_ "button"
            , makeAttributes "data-copy" (scdSignature card)
            , title_ "Copy signature"
            ]
            "Copy"
  pre_ [class_ "signature"] (code_ (toHtml (scdSignature card)))
  div_ [class_ "haddock"] (Haddock.renderHaddockHtml (scdHaddock card))
  div_ [class_ "src-link"] $ do
    a_ [ href_ ("/pkg/" <> pkg <> "/" <> scdModule card
                 <> "#" <> anchorFor kind name)
       ]
       "View in module"
    toHtml (" \x00B7 source: " :: Text)
    case scdLine card of
      Just srcLine ->
        a_ [ href_ ("/source/" <> pkg <> "/" <> scdModule card
                     <> "?line=" <> Text.pack (show srcLine)
                     <> "#L"     <> Text.pack (show srcLine))
           ]
           (toHtml (pkg <> "/" <> scdModule card <> ":" <> Text.pack (show srcLine)))
      Nothing ->
        a_ [ href_ ("/source/" <> pkg <> "/" <> scdModule card) ]
           (toHtml (pkg <> "/" <> scdModule card))
  where
    -- Unclassified symbols default to the value namespace: correct for
    -- everything except a type we failed to parse, and a wrong @t:@
    -- guess would break value anchors far more often.
    kind = maybe DkFunction id (scdKind card)
