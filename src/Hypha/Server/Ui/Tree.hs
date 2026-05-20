{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Server.Ui.Tree
  ( packageTree
  , originBadge
  , originBadgeFull
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import Lucid

import Hypha.Types.BuildPlan (PackageOrigin (..))

-- | Sidebar package list linking to each component's overview page.
-- Each row places a single-letter origin chip to the left of the
-- package label so the chip and the link stay on one row.  The
-- composite name still carries the @:exe:@ or @:sublib@ tag at the
-- tail; only the leading chip is new.
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
      in li_ [class_ "tree-row"] $ do
           originBadge origin
           a_ [href_ ("/pkg/" <> hrefText)] $ do
             toHtml pkgPart
             case suffix of
               Nothing -> pure ()
               Just s  -> span_ [class_ kindCls] (toHtml s)

-- | Single-letter provenance chip used in the sidebar.  Always rendered
-- so every package row has the same horizontal layout; the chip's
-- background colour distinguishes the origin without taking real
-- estate from the package name.
originBadge :: PackageOrigin -> Html ()
originBadge o =
  let (cls, letter, label) = originChip o
  in span_ [ class_ ("origin-chip " <> cls)
           , title_ label
           ] (toHtml letter)

-- | Fully spelt-out origin block for the package page header.  Used by
-- 'pkgPage' so a clicked package shows the chip's meaning in plain
-- English plus any SRP metadata the plan recorded.
originBadgeFull :: PackageOrigin -> Html ()
originBadgeFull o =
  let (cls, _letter, label) = originChip o
  in div_ [class_ ("origin-pill " <> cls)] $ do
       toHtml label
       case o of
         OriginSourceRepo url ref subdir -> originDetails url ref subdir
         OriginLocal p                   -> span_ [class_ "origin-detail"]
                                              (toHtml (" \x2014 " <> Text.pack p))
         OriginLocalTarball p            -> span_ [class_ "origin-detail"]
                                              (toHtml (" \x2014 " <> Text.pack p))
         OriginRemoteTarball u           -> span_ [class_ "origin-detail"]
                                              (toHtml (" \x2014 " <> u))
         _                                -> pure ()

originDetails :: Maybe Text -> Maybe Text -> Maybe FilePath -> Html ()
originDetails url ref subdir =
  let pieces = mconcat
        [ maybe [] (\u -> [u])        url
        , maybe [] (\r -> ["@" <> r]) ref
        , maybe [] (\s -> ["(" <> Text.pack s <> ")"]) subdir
        ]
  in case pieces of
       [] -> pure ()
       _  -> span_ [class_ "origin-detail"]
               (toHtml (" \x2014 " <> Text.intercalate " " pieces))

-- | (CSS class, single-letter label, full label) tuple for an origin.
originChip :: PackageOrigin -> (Text, Text, Text)
originChip = \case
  OriginHackage          -> ("hackage", "H", "Hackage")
  OriginDistribution     -> ("distribution", "D", "GHC distribution (boot library)")
  OriginLocal _          -> ("local",   "L", "Local package")
  OriginLocalTarball _   -> ("tarball", "T", "Local tarball")
  OriginRemoteTarball _  -> ("tarball", "T", "Remote tarball")
  OriginSourceRepo _ _ _ -> ("srp",     "S", "source-repository-package")
