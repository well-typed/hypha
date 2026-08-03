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
import           Hypha.Types.Route qualified as Route

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
  case scdHaddock card of
    Just hd -> div_ [class_ "haddock"] (Haddock.renderHaddockHtml hd)
    Nothing -> mempty
  div_ [class_ "src-link"] $ do
    a_ [ href_ (Route.hrefFrom ["pkg", defPkg, scdModule card]
                 <> "#" <> anchorFor kind name)
       ]
       "View in module"
    toHtml (" \x00B7 source: " :: Text)
    case scdLine card of
      Just srcLine ->
        a_ [ href_ (Route.hrefFrom ["source", defPkg, scdModule card]
                     <> "?line=" <> Text.pack (show srcLine)
                     <> "#L"     <> Text.pack (show srcLine))
           ]
           (toHtml (defPkg <> "/" <> scdModule card <> ":" <> Text.pack (show srcLine)))
      Nothing ->
        a_ [ href_ (Route.hrefFrom ["source", defPkg, scdModule card]) ]
           (toHtml (defPkg <> "/" <> scdModule card))
  where
    -- Carried as the 'Maybe' it is.  Defaulting an unclassified symbol to
    -- the value namespace was a guess presented as fact, and it produced
    -- a @#v:@ anchor for types that no @#t:@ link resolves to.
    kind = scdKind card

    -- Links to the definition are built from the component that /defines/
    -- the symbol, not the one the URL asked for.  A re-export can cross a
    -- package boundary, and @\/pkg\/base\/GHC.Internal…@ is a module @base@
    -- does not have.
    defPkg = scdComponent card

    -- The card is reached through the module the user asked for, but the
    -- code lives where it is declared.  Saying both is the difference
    -- between an honest card and one that quietly relabels itself.  The
    -- package is named only when it differs, so a same-package re-export
    -- reads exactly as it did before.
    reexportNote
      | scdRequested card == scdModule card && defPkg == pkg = mempty
      | otherwise = p_ [class_ "hint"] $ do
          toHtml ("Re-exported by " :: Text)
          code_ (toHtml (scdRequested card))
          toHtml (", defined in " :: Text)
          code_ (toHtml definedIn)
          toHtml ("." :: Text)

    definedIn
      | defPkg == pkg = scdModule card
      | otherwise     = defPkg <> ":" <> scdModule card

