{-# LANGUAGE OverloadedStrings #-}
module Hypha.Server.Haddock.Rewrite
  ( rewriteHaddockHtml
  ) where

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
    fixTag (TS.TagOpen "a" attrs) = TS.TagOpen "a" (map fixHref attrs)
    fixTag t                      = t

    fixHref ("href", v) = ("href", fixUrl v)
    fixHref kv          = kv

    fixUrl v
      | "../" `Text.isPrefixOf` v =
          let rest = Text.drop 3 v
          in case Text.splitOn "/" rest of
               (dir:_) | dirLooksLikePkg dir -> "/haddock/" <> rest
               _                             -> v
      | otherwise = v

    dirLooksLikePkg :: Text -> Bool
    dirLooksLikePkg d =
      -- Heuristic: a pkg-ver token has at least one '-' and the suffix starts
      -- with a digit (the version).
      case Text.splitOn "-" d of
        (_ : rest@(_ : _)) -> case Text.uncons (last rest) of
          Just (c, _) -> c >= '0' && c <= '9'
          Nothing     -> False
        _                -> False
