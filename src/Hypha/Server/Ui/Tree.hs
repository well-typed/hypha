{-# LANGUAGE OverloadedStrings #-}
module Hypha.Server.Ui.Tree
  ( packageTree
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import Lucid

-- | Sidebar package list linking to each component's overview page.
-- Entries are composite names of the form @pkg@, @pkg:sublib@, or
-- @pkg:exe:name@; the trailing tag is rendered in a muted span and
-- the URL has its @:@ percent-encoded.
packageTree :: [Text] -> Html ()
packageTree = ul_ [class_ "tree"] . mconcat . map renderEntry
  where
    renderEntry :: Text -> Html ()
    renderEntry compName =
      let (pkgPart, tail_) = case Text.breakOn ":" compName of
            (a, b) | Text.null b -> (a, Nothing)
                   | otherwise   -> (a, Just (Text.drop 1 b))
          (kindCls, suffix, hrefSuffix) = case tail_ of
            Nothing  -> ("", Nothing, "")
            Just t   -> case Text.stripPrefix "exe:" t of
              Just e  -> ("exe-tag",    Just (":exe:" <> e), "%3Aexe%3A" <> e)
              Nothing -> ("sublib-tag", Just (":"     <> t), "%3A"       <> t)
          hrefText = pkgPart <> hrefSuffix
      in li_ $ a_ [href_ ("/pkg/" <> hrefText)] $ do
           toHtml pkgPart
           case suffix of
             Nothing -> pure ()
             Just s  -> span_ [class_ kindCls] (toHtml s)
