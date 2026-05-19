{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Render Haddock comment prose to HTML for the server's symbol card.
--
-- The 'Hypha.Source.Extract' step grabs raw comment text — leading
-- @-- |@ / @-- ^@ markers, line-by-line.  We strip those markers, hand
-- the cleaned-up text to 'haddock-library', and walk the resulting
-- 'DocH' AST into Lucid HTML.  No external CSS/JS dependency: the
-- elements we emit (p, code, pre, em, strong, ul, ol, a, h1\x2026h3,
-- table) are styled by the existing components stylesheet.
module Hypha.Server.Ui.Haddock
  ( renderHaddockHtml
  , stripCommentMarkers
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Documentation.Haddock.Parser as HP
import qualified Documentation.Haddock.Types  as HT
import Lucid

-- | Parse the raw comment text and render to Lucid HTML.  Empty input
-- emits nothing so the card collapses cleanly when a symbol has no
-- Haddock prose.
renderHaddockHtml :: Text -> Html ()
renderHaddockHtml raw
  | Text.null cleaned = mempty
  | otherwise         = fromDocH (HP.toRegular (HP.parseString (Text.unpack cleaned)))
  where
    cleaned = Text.strip (stripCommentMarkers raw)

-- | Drop the leading @--@/@-- |@/@-- ^@ markers from every line.  The
-- @ ^@ variant marks documentation attached to the /previous/ binding,
-- but for our purposes both should render identically.
stripCommentMarkers :: Text -> Text
stripCommentMarkers = Text.unlines . map stripOne . Text.lines
  where
    stripOne :: Text -> Text
    stripOne line =
      let trimmed = Text.dropWhile (`elem` (" \t" :: String)) line
      in case Text.stripPrefix "--" trimmed of
           Just rest -> Text.dropWhile (== ' ') (dropMarker rest)
           Nothing   -> line

    dropMarker :: Text -> Text
    dropMarker t =
      case Text.uncons t of
        Just (' ', rest) -> case Text.uncons rest of
          Just ('|', r) -> r
          Just ('^', r) -> r
          _             -> Text.cons ' ' rest
        Just ('|', rest) -> rest
        Just ('^', rest) -> rest
        _                -> t

-- | Walk a Haddock 'DocH' AST into Lucid HTML.  We collapse identifier
-- references to their textual form via 'HP.toRegular' so the identifier
-- type is plain 'String'; the module-name slot ('mod') is left
-- polymorphic and ignored — the symbol card has nowhere to link it.
fromDocH :: HT.DocH mod String -> Html ()
fromDocH = \case
  HT.DocEmpty        -> mempty
  HT.DocAppend x y   -> fromDocH x <> fromDocH y
  HT.DocString s     -> toHtml (Text.pack s)
  HT.DocParagraph x  -> p_ (fromDocH x)
  HT.DocIdentifier s ->
    code_ (toHtml (Text.pack s))
  HT.DocIdentifierUnchecked _ ->
    code_ (toHtml ("\x2026" :: Text))
  HT.DocModule m     -> code_ (toHtml (Text.pack (HT.modLinkName m)))
  HT.DocEmphasis x   -> em_ (fromDocH x)
  HT.DocBold x       -> strong_ (fromDocH x)
  HT.DocMonospaced x -> code_ (fromDocH x)
  HT.DocCodeBlock x  -> pre_ (code_ (fromDocH x))
  HT.DocHyperlink h  ->
    let url   = Text.pack (HT.hyperlinkUrl h)
        label = maybe (toHtml url) fromDocH (HT.hyperlinkLabel h)
    in a_ [href_ url] label
  HT.DocPic _        -> mempty
  HT.DocAName _      -> mempty
  HT.DocProperty s   -> pre_ (code_ (toHtml (Text.pack s)))
  HT.DocExamples es  ->
    pre_ $ code_ $ mapM_ (\ex ->
      do toHtml (Text.pack (">>> " <> HT.exampleExpression ex <> "\n"))
         mapM_ (\r -> toHtml (Text.pack (r <> "\n"))) (HT.exampleResult ex)
      ) es
  HT.DocHeader h     -> case HT.headerLevel h of
    1 -> h1_ (fromDocH (HT.headerTitle h))
    2 -> h2_ (fromDocH (HT.headerTitle h))
    _ -> h3_ (fromDocH (HT.headerTitle h))
  HT.DocTable _      -> mempty
  HT.DocUnorderedList xs -> ul_ (mapM_ (li_ . fromDocH) xs)
  HT.DocOrderedList xs ->
    ol_ (mapM_ (li_ . fromDocH . snd) xs)
  HT.DocDefList xs   ->
    dl_ (mapM_ (\(k, v) -> dt_ (fromDocH k) <> dd_ (fromDocH v)) xs)
  HT.DocMathInline s  -> code_ (toHtml (Text.pack s))
  HT.DocMathDisplay s -> pre_ (code_ (toHtml (Text.pack s)))
  HT.DocWarning x     -> div_ [class_ "warn"] (fromDocH x)

