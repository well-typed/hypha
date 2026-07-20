{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
-- | Slice the useful content regions out of a prebuilt Haddock module
-- page so the server can embed real documentation inside its own
-- shell instead of iframing (or ignoring) the original page.
--
-- Haddock's HTML has kept the same skeleton for years:
--
-- > <div id="content">
-- >   <div id="module-header">…</div>
-- >   <div id="table-of-contents">…</div>
-- >   <div id="description">…</div>
-- >   <div id="synopsis">…</div>
-- >   <div id="interface">…</div>
-- > </div>
--
-- We keep the description (module prose), the interface (the actual
-- declarations), and the contents list (feeds the page's TOC rail).
-- The synopsis is dropped: it duplicates the interface and depends on
-- Haddock's own JS.  A page without a @#interface@ region is treated
-- as unknown markup and rejected so the caller can degrade to
-- source-rendered documentation instead of embedding garbage.
module Hypha.Server.Haddock.Extract
  ( PrebuiltParts (..)
  , extractModuleDocHtml
  ) where

import Data.Text (Text)
import qualified Text.HTML.TagSoup as TS

-- | Inner HTML of the three regions we embed.
data PrebuiltParts = PrebuiltParts
  { ppDescription :: !(Maybe Text)
    -- ^ Inner HTML of @div#description@.
  , ppInterface   :: !Text
    -- ^ Inner HTML of @div#interface@ — the declarations.
  , ppContents    :: !(Maybe Text)
    -- ^ Inner HTML of @div#table-of-contents@.
  }
  deriving stock (Show, Eq)

-- | Extract the embeddable regions from a full Haddock module page.
-- 'Nothing' iff the @#interface@ region is missing.
extractModuleDocHtml :: Text -> Maybe PrebuiltParts
extractModuleDocHtml html =
  case sliceDiv "interface" tags of
    Nothing    -> Nothing
    Just iface -> Just PrebuiltParts
      { ppDescription = TS.renderTags <$> sliceDiv "description" tags
      , ppInterface   = TS.renderTags iface
      , ppContents    = TS.renderTags <$> sliceDiv "table-of-contents" tags
      }
  where
    tags = TS.parseTags html

-- | The tag stream strictly between @\<div id=target\>@ and its
-- /matching/ close tag (nested divs tracked by depth).
sliceDiv :: Text -> [TS.Tag Text] -> Maybe [TS.Tag Text]
sliceDiv target = go
  where
    go []                                = Nothing
    go (TS.TagOpen "div" attrs : rest)
      | lookup "id" attrs == Just target = Just (inner (0 :: Int) rest)
    go (_ : rest)                        = go rest

    inner _ [] = []
    inner depth (t : rest) = case t of
      TS.TagOpen  "div" _ -> t : inner (depth + 1) rest
      TS.TagClose "div"
        | depth == 0      -> []
        | otherwise       -> t : inner (depth - 1) rest
      _                   -> t : inner depth rest
