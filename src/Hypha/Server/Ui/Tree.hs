{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Server.Ui.Tree
  ( sidebar
  , packageTree
  , splitByOrigin
  , originBadge
  , originBadgeFull
  , hackageLink
  ) where

import Data.List (partition)
import Data.Text (Text)
import qualified Data.Text as Text
import Lucid

import Hypha.Types.BuildPlan (PackageOrigin (..))
import Hypha.Types.PackageId (PackageName (..), Version (..))
import Hypha.Types.Route qualified as Route

-- | Full sidebar: a client-side filter box above two collapsible
-- groups — the project's own packages first, dependencies below.
-- Empty groups are omitted entirely.
sidebar :: [(Text, PackageOrigin)] -> Html ()
sidebar pkgs = do
  input_ [ class_ "tree-filter"
         , type_ "search"
         , placeholder_ "Filter packages\x2026"
         ]
  let (proj, deps) = splitByOrigin pkgs
  group "Project" proj
  group "Dependencies" deps
  where
    group _ [] = mempty
    group label items = details_ [open_ ""] $ do
      summary_ [class_ "tree-group"] $ do
        toHtml (label :: Text)
        span_ [class_ "count"] (toHtml (Text.pack (show (length items))))
      packageTree items

-- | Partition sidebar entries into (project packages, dependencies).
-- Only 'OriginLocal' counts as project: source-repository-package and
-- tarball entries are pinned third-party code, not the project itself.
splitByOrigin
  :: [(Text, PackageOrigin)]
  -> ([(Text, PackageOrigin)], [(Text, PackageOrigin)])
splitByOrigin = partition (isLocal . snd)
  where
    isLocal (OriginLocal _) = True
    isLocal _               = False

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
          -- The split is for the chip: the href is the component key
          -- whole, escaped once by 'Route.hrefFrom' like every other link
          -- on the site.  Hand-escaping the colons here was the one place
          -- that spelled the same URL differently.
          (kindCls, suffix) = case tail_ of
            Nothing  -> ("", Nothing)
            Just t   -> case Text.stripPrefix "exe:" t of
              Just e  -> ("exe-tag",    Just (":exe:" <> e))
              Nothing -> ("sublib-tag", Just (":"     <> t))
      in li_ [class_ "tree-row"] $ do
           originBadge origin
           a_ [href_ (Route.hrefFrom ["pkg", compName])] $ do
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

-- | Right-aligned \"view on Hackage\" link for the package page header.
--
-- Hackage-sourced /and/ distribution packages both get one: @containers@,
-- @base@ and every other library shipped with GHC is published on Hackage
-- at the version the plan pins, so withholding the link there was simply
-- wrong.  A boot library from an unreleased GHC can 404, which is rare and
-- honest.
--
-- Local, source-repo and tarball packages still get nothing: for those the
-- version does not identify a Hackage listing, and a link would send the
-- user to a 404 or, worse, to someone else's same-named package.
--
-- The match is per-constructor rather than a catch-all, so a new origin
-- fails to compile here instead of silently losing its link.
hackageLink :: PackageName -> Version -> PackageOrigin -> Html ()
hackageLink pkg ver origin = case origin of
  OriginHackage         -> link
  OriginDistribution    -> link
  OriginSourceRepo{}    -> mempty
  OriginLocal{}         -> mempty
  OriginLocalTarball{}  -> mempty
  OriginRemoteTarball{} -> mempty
  where
    link =
      a_ [ class_ "hackage-link"
         , href_ ("https://hackage.haskell.org/package/"
                    <> unPackageName pkg <> "-" <> unVersion ver)
         , target_ "_blank"
         , rel_ "noopener"
         ]
         "\x2197 Hackage"

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
