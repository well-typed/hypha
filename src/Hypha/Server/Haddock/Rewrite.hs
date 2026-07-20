{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
module Hypha.Server.Haddock.Rewrite
  ( rewriteHaddockHtml
  , EmbedContext (..)
  , rewriteEmbeddedDocHtml
  ) where

import Control.Monad (guard)
import Data.Char (isDigit, isUpper)
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Text.HTML.TagSoup as TS

-- | Rewrite cross-package relative URLs of the form @../<pkg>-<ver>/...@
-- to absolute server routes @/haddock/<pkg>-<ver>/...@. Anchors and
-- same-page @#frag@ links are preserved.  Used when serving a full
-- Haddock page verbatim under @/haddock/@.
rewriteHaddockHtml :: Text -> Text
rewriteHaddockHtml = rewriteWith [("a", "href")] crossPackageUrl

-- | Where a Haddock fragment is being embedded: the shell page lives at
-- @/pkg/\<component\>/\<module\>@, so sibling-module links must be
-- re-rooted onto @/pkg/@ routes and package-relative assets onto the
-- @/haddock/\<pkg-ver\>/@ route.
data EmbedContext = EmbedContext
  { ecComponent :: !Text
    -- ^ URL component name, e.g. @containers@ or @hypha:sublib@.
  , ecPkgVer    :: !Text
    -- ^ @\<pkg\>-\<ver\>@ token, e.g. @containers-0.7@.
  }
  deriving stock (Show, Eq)

-- | Rewrite URLs inside an /extracted/ Haddock fragment so they still
-- resolve when the fragment is embedded on a @/pkg/@ page:
--
--   * @../\<pkg\>-\<ver\>/...@   → @/haddock/\<pkg\>-\<ver\>/...@   (cross-package)
--   * @src/...@              → @/haddock/\<pkg-ver\>/src/...@  (source pages)
--   * @Data-Map-Strict.html@ → @/pkg/\<component\>/Data.Map.Strict@ (sibling module)
--   * other relative @.html@ → @/haddock/\<pkg-ver\>/...@      (index pages)
--   * @#frag@, absolute, and scheme-qualified URLs are untouched.
--
-- Applied to @href@ on @\<a\>@ and @src@ on @\<img\>@.
rewriteEmbeddedDocHtml :: EmbedContext -> Text -> Text
rewriteEmbeddedDocHtml ctx =
  rewriteWith [("a", "href"), ("img", "src")] (embeddedUrl ctx)

-- | Shared tag traversal: rewrite attribute @attr@ on tag @tag@ for
-- every @(tag, attr)@ pair, using the given URL fixer.
rewriteWith :: [(Text, Text)] -> (Text -> Text) -> Text -> Text
rewriteWith targets fixUrl html =
  TS.renderTags (map fixTag (TS.parseTags html))
  where
    fixTag (TS.TagOpen name attrs)
      | Just attr <- lookup name targets =
          TS.TagOpen name (map (fixAttr attr) attrs)
    fixTag t = t

    fixAttr attr (k, v) | k == attr = (k, fixUrl v)
    fixAttr _    kv                 = kv

-- | Replace a @../<pkg>-<ver>/...@ relative URL with an absolute server
-- route @/haddock/<pkg>-<ver>/...@. Non-matching URLs are left as-is.
crossPackageUrl :: Text -> Text
crossPackageUrl v = fromMaybe v $ do
  -- Attempt to strip the "../" prefix; if it doesn't start with "../",
  -- this is a same-page anchor or absolute URL — leave unchanged.
  rest <- Text.stripPrefix "../" v
  let dir = Text.takeWhile (/= '/') rest
  guard (dirLooksLikePkg dir)
  pure ("/haddock/" <> rest)

-- | URL fixer for embedded fragments; see 'rewriteEmbeddedDocHtml'.
embeddedUrl :: EmbedContext -> Text -> Text
embeddedUrl ctx v
  | Text.null v                 = v
  | "#" `Text.isPrefixOf` v     = v
  | "/" `Text.isPrefixOf` v     = v
  | hasScheme v                 = v
  | "../" `Text.isPrefixOf` v   = crossPackageUrl v
  | "src/" `Text.isPrefixOf` v  = "/haddock/" <> ecPkgVer ctx <> "/" <> v
  | otherwise =
      case Text.breakOn "#" v of
        (path, frag)
          | Just dotted <- siblingModule path ->
              "/pkg/" <> ecComponent ctx <> "/" <> dotted <> frag
          | isRelativeHtml path ->
              "/haddock/" <> ecPkgVer ctx <> "/" <> v
          | otherwise -> v
  where
    -- "http://", "https://", "mailto:", "data:", ...
    hasScheme t =
      let (scheme, rest) = Text.breakOn ":" t
      in not (Text.null rest)
           && not (Text.null scheme)
           && Text.all (`notElem` ("/#?" :: String)) scheme

    -- A sibling module page: "Data-Map-Strict.html" — no path
    -- separator, ".html" suffix, uppercase stem initial.  Module
    -- names cannot contain hyphens, so every hyphen is a Haddock
    -- dot-encoding and the reverse mapping is total.
    siblingModule p = do
      stem <- Text.stripSuffix ".html" p
      guard (not (Text.any (== '/') p))
      (c, _) <- Text.uncons stem
      guard (isUpper c)
      pure (Text.replace "-" "." stem)

    -- Other package-relative pages (index.html, doc-index.html):
    -- keep them working via the raw /haddock/ route.
    isRelativeHtml p =
      ".html" `Text.isSuffixOf` p && not (Text.any (== '/') p)

-- | Heuristic: a @pkg-ver@ token has at least one @-@ and the suffix
-- after the last @-@ starts with a digit (the version component).
-- This is an approximation — it may yield false positives on directory
-- names like @some-dir-123@. Golden tests guard against regressions.
dirLooksLikePkg :: Text -> Bool
dirLooksLikePkg d
  | _ : rest@(_ : _) <- Text.splitOn "-" d
  , Just (c, _)      <- Text.uncons (last rest)
  = isDigit c
dirLooksLikePkg _ = False
