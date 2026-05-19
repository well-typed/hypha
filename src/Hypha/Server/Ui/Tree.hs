{-# LANGUAGE OverloadedStrings #-}
module Hypha.Server.Ui.Tree
  ( packageTree
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import Lucid

-- | Sidebar package list linking to each component's overview page.
-- Entries are composite names of the form @pkg@ or @pkg:sublib@; the
-- sub-library part is rendered as a muted suffix and the URL has its
-- @:@ percent-encoded so Servant's @Capture@ receives a single
-- segment.
packageTree :: [Text] -> Html ()
packageTree = ul_ [class_ "tree"] . mconcat . map renderEntry
  where
    renderEntry :: Text -> Html ()
    renderEntry compName =
      let (pkgPart, sublib) = case Text.breakOn ":" compName of
            (a, b) | Text.null b -> (a, Nothing)
                   | otherwise   -> (a, Just (Text.drop 1 b))
          hrefText = case sublib of
            Nothing -> pkgPart
            Just s  -> pkgPart <> "%3A" <> s
      in li_ $ a_ [href_ ("/pkg/" <> hrefText)] $ do
           toHtml pkgPart
           case sublib of
             Nothing -> pure ()
             Just s  -> span_ [class_ "sublib-tag"] (toHtml (":" <> s))
