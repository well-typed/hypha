{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Server.Haddock.Rewrite
  ( rewriteHaddockHtml
  ) where

import Control.Monad (guard)
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Text.HTML.TagSoup as TS

-- | Rewrite cross-package relative URLs of the form @../<pkg>-<ver>/...@
-- to absolute server routes @/haddock/<pkg>-<ver>/...@. Anchors and
-- same-page @#frag@ links are preserved.
rewriteHaddockHtml :: Text -> Text
rewriteHaddockHtml html =
  TS.renderTags (map fixTag (TS.parseTags html))
  where
    -- Currently only rewrites @<a href="..."@ attributes. Haddock may also
    -- emit @<link@ and @<img@ with relative cross-package hrefs. Extend
    -- @fixAttrs@ to cover those if asset rewriting is needed.
    fixTag (TS.TagOpen "a" attrs) = TS.TagOpen "a" (map fixHref attrs)
    fixTag t                      = t

    fixHref ("href", v) = ("href", fixUrl v)
    fixHref kv          = kv

    -- | Replace a @../<pkg>-<ver>/...@ relative URL with an absolute server
    -- route @/haddock/<pkg>-<ver>/...@. Non-matching URLs are left as-is.
    fixUrl :: Text -> Text
    fixUrl v = fromMaybe v $ do
      -- Attempt to strip the "../" prefix; if it doesn't start with "../",
      -- this is a same-page anchor or absolute URL — leave unchanged.
      rest <- Text.stripPrefix "../" v
      let dir = Text.takeWhile (/= '/') rest
      guard (dirLooksLikePkg dir)
      pure ("/haddock/" <> rest)

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
