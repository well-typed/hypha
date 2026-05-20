{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Server.Ui.Tree
  ( packageTree
  , originBadge
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import Lucid

import Hypha.Types.BuildPlan (PackageOrigin (..))

-- | Sidebar package list linking to each component's overview page.
-- Entries are @(compositeName, origin)@ where the name is one of
-- @pkg@, @pkg:sublib@, or @pkg:exe:name@.  The trailing kind tag is
-- rendered as a muted span; the @origin@ value drives a small
-- provenance badge so users can tell a fork apart from the Hackage
-- copy of the same @pkg-ver@.
packageTree :: [(Text, PackageOrigin)] -> Html ()
packageTree = ul_ [class_ "tree"] . mconcat . map renderEntry
  where
    renderEntry :: (Text, PackageOrigin) -> Html ()
    renderEntry (compName, origin) =
      let (pkgPart, tail_) = case Text.breakOn ":" compName of
            (a, b) | Text.null b -> (a, Nothing)
                   | otherwise   -> (a, Just (Text.drop 1 b))
          (kindCls, suffix, hrefSuffix) = case tail_ of
            Nothing  -> ("", Nothing, "")
            Just t   -> case Text.stripPrefix "exe:" t of
              Just e  -> ("exe-tag",    Just (":exe:" <> e), "%3Aexe%3A" <> e)
              Nothing -> ("sublib-tag", Just (":"     <> t), "%3A"       <> t)
          hrefText = pkgPart <> hrefSuffix
      in li_ $ do
           a_ [href_ ("/pkg/" <> hrefText)] $ do
             toHtml pkgPart
             case suffix of
               Nothing -> pure ()
               Just s  -> span_ [class_ kindCls] (toHtml s)
           originBadge origin

-- | Tiny inline pill summarising package provenance.  Hidden when the
-- origin would simply read \"hackage\" — the silent default avoids
-- visual noise in projects whose plan is entirely Hackage-pinned.
originBadge :: PackageOrigin -> Html ()
originBadge = \case
  OriginHackage          -> pure ()
  OriginUnknown          -> pure ()
  OriginLocal _          ->
    span_ [class_ "origin-tag local"
          , title_ "local package"] "local"
  OriginLocalTarball _   ->
    span_ [class_ "origin-tag tarball"
          , title_ "local tarball"] "tarball"
  OriginRemoteTarball _  ->
    span_ [class_ "origin-tag tarball"
          , title_ "remote tarball"] "tarball"
  OriginSourceRepo _ tag _ ->
    let lbl = case tag of
          Just t  -> "srp@" <> Text.take 7 t
          Nothing -> "srp"
    in span_ [class_ "origin-tag srp", title_ "source-repository-package"]
         (toHtml lbl)
