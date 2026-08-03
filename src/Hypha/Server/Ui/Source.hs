{-# LANGUAGE OverloadedStrings #-}
module Hypha.Server.Ui.Source
  ( sourceView
  ) where

import qualified Data.Text as Text
import Data.Text (Text)
import Lucid
import Lucid.Base (makeAttributes)
import qualified Skylighting as Sky

import Hypha.Types.Route qualified as Route

-- | Render a Haskell source listing with line numbers, optional highlighted
-- target line (@mLine@), and skylighting-driven token colouring.  Falls
-- back to a plain @<pre>@ when the syntax description is missing — the
-- view never blocks on a non-fatal parse.
sourceView
  :: Text        -- ^ package (URL component, for the docs backlink)
  -> Text        -- ^ module path (used for the heading)
  -> Maybe Int   -- ^ optional line to highlight + scroll to (@#L\<n\>@)
  -> Text        -- ^ full source body
  -> Html ()
sourceView pkg modPath mLine body = div_ [class_ "source-view"] $ do
  div_ [class_ "source-head"] $ do
    span_ [class_ "source-title"] (toHtml modPath)
    button_ [ class_ "copy-btn"
            , type_ "button"
            , makeAttributes "data-copy" modPath
            , title_ "Copy module path"
            ]
            "Copy"
    maybe mempty (\n -> span_ [class_ "source-line-hint"]
                          (toHtml ("line " <> Text.pack (show n)))) mLine
    a_ [ class_ "source-docs-link"
       , href_ (Route.hrefFrom ["pkg", pkg, modPath])
       ]
       "\x2190 Docs"
  table_ [class_ "source"] $
    tbody_ $ mapM_ row (numbered body)
  where
    row :: (Int, Text) -> Html ()
    row (n, ln) =
      let lineClass = case mLine of
                        Just t | t == n -> class_ "src-line target"
                        _                -> class_ "src-line"
      in tr_ [lineClass, id_ ("L" <> Text.pack (show n))] $ do
        td_ [class_ "ln"] $
          a_ [href_ ("#L" <> Text.pack (show n))] (toHtml (show n))
        td_ [class_ "code"] $ pre_ (highlightLine ln)

    highlightLine :: Text -> Html ()
    highlightLine ln = case Sky.lookupSyntax "Haskell" Sky.defaultSyntaxMap of
      Nothing  -> toHtml ln
      Just syn -> case Sky.tokenize tokCfg syn ln of
        Right toks -> mconcat (map renderSourceLine toks)
        Left _     -> toHtml ln
    tokCfg = Sky.TokenizerConfig
      { Sky.syntaxMap   = Sky.defaultSyntaxMap
      , Sky.traceOutput = False
      }

-- | Render a single skylighting 'SourceLine' (a list of tokens) as Html
-- spans tagged with a class per token type.
renderSourceLine :: Sky.SourceLine -> Html ()
renderSourceLine = mapM_ renderToken

renderToken :: Sky.Token -> Html ()
renderToken (tt, txt) =
  span_ [class_ ("tok-" <> Text.pack (drop 0 (show tt)))] (toHtml txt)

-- | Pair source lines with 1-based indices.  Handles an empty file too:
-- the body always rendered as at least one (empty) row, so the @<table>@
-- stays well-formed.
numbered :: Text -> [(Int, Text)]
numbered t =
  let ls = Text.lines t
  in zip [1 ..] (if null ls then [""] else ls)
