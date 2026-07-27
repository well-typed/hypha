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
import           Hypha.Source.Locate (Provenance (..))
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
    case scdSignature card of
      Nothing  -> mempty
      Just sig ->
        button_ [ class_ "copy-btn"
                , type_ "button"
                , makeAttributes "data-copy" sig
                , title_ "Copy signature"
                ]
                "Copy"
  -- An absent signature says so.  It used to render as an empty box,
  -- indistinguishable from a signature we failed to read.
  case scdSignature card of
    Just sig -> pre_ [class_ "signature"] (code_ (toHtml sig))
    Nothing  -> p_ [class_ "hint"]
      (toHtml ("No signature in the source for this binding." :: Text))
  reexportNote
  provenanceNote
  case scdHaddock card of
    Just hd -> div_ [class_ "haddock"] (Haddock.renderHaddockHtml hd)
    Nothing -> mempty
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

    -- The card is reached through the module the user asked for, but the
    -- code lives where it is declared.  Saying both is the difference
    -- between an honest card and one that quietly relabels itself.
    reexportNote
      | scdRequested card == scdModule card = mempty
      | otherwise = p_ [class_ "hint"] $ do
          toHtml ("Re-exported by " :: Text)
          code_ (toHtml (scdRequested card))
          toHtml (", defined in " :: Text)
          code_ (toHtml (scdModule card))
          toHtml ("." :: Text)

    -- A swept location is a guess.  Rendering it identically to a resolved
    -- one is how the wrong file came to look like fact.
    provenanceNote = case scdProvenance card of
      Resolved _         -> mempty
      GuessedBySweep why -> p_ [class_ "warn"] $ do
        toHtml ("Best guess: this location was found by scanning the package, \
                \not resolved from its exports (" :: Text)
        toHtml why
        toHtml (")." :: Text)
