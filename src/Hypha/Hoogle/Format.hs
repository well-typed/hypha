{-# LANGUAGE OverloadedStrings #-}
-- | Shared post-processing for raw Hoogle output (local-DB and
-- remote-HTTP).  The @hoogle@ library and the public web service
-- both wrap symbol names in @\<span class=name\>...\</span\>@ and
-- HTML-encode entities (@&gt;@).  We strip both before handing
-- hits to callers so the JSON envelope is agent-friendly.
module Hypha.Hoogle.Format
  ( stripTags
  , decodeEntities
  , splitNameSig
  ) where

import Data.Text (Text)
import qualified Data.Text as Text

-- | Drop every @\<…\>@ run.  Cheap; Hoogle's HTML payload is shallow
-- (no nested attributes carrying @\>@).
stripTags :: Text -> Text
stripTags = Text.pack . go . Text.unpack
  where
    go []         = []
    go ('<' : rs) = go (drop 1 (dropWhile (/= '>') rs))
    go (c : rs)   = c : go rs

-- | Tiny HTML-entity decoder covering the entities Hoogle actually
-- emits (@&lt; &gt; &amp; &quot; &#39;@).
decodeEntities :: Text -> Text
decodeEntities = Text.pack . go . Text.unpack
  where
    go [] = []
    go ('&':rest)
      | Just (c, rs) <- entity rest = c : go rs
    go (c:rs) = c : go rs

    entity s
      | Just rs <- prefix "lt;"   s = Just ('<',  rs)
      | Just rs <- prefix "gt;"   s = Just ('>',  rs)
      | Just rs <- prefix "amp;"  s = Just ('&',  rs)
      | Just rs <- prefix "quot;" s = Just ('"',  rs)
      | Just rs <- prefix "#39;"  s = Just ('\'', rs)
      | otherwise                   = Nothing

    prefix p s
      | take (length p) s == p = Just (drop (length p) s)
      | otherwise              = Nothing

-- | Split @\"id :: a -> a\"@ → @(\"id\", \"a -> a\")@.  When the
-- separator is absent the entire string is the name and the
-- signature is empty.
splitNameSig :: Text -> (Text, Text)
splitNameSig t = case Text.breakOn " :: " t of
  (name, rest)
    | Text.null rest -> (Text.strip name, "")
    | otherwise      -> (Text.strip name, Text.strip (Text.drop 4 rest))
