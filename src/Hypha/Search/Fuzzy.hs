{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Tiny, dependency-free fuzzy scorer used by the live search index.
--
-- The query is split into whitespace-separated tokens.  Every token must
-- hit somewhere on the row (otherwise the row is filtered out).  Each
-- token earns a per-field score: matches against the symbol name beat
-- matches against the module path, which beat matches against the package
-- name.  Within a field, contiguous substring hits beat subsequence hits,
-- prefix hits beat infix hits, and shorter names break ties.
--
-- That structure gives us FZF/Telescope-style behaviour without dragging
-- in a fuzzy-matching library: \"Data.Map.Strict.lookup\" lands on the
-- exact symbol, \"Data.Map lookup\" lands close to it, and \"lookup\"
-- still surfaces the short variants first.
--
-- Performance: rows are scored on every keystroke, so the lowercased
-- fields used for matching are precomputed at index-build time
-- ('mkIndexedRow').  The hot path only does pure 'Text.isInfixOf' /
-- 'Text.isPrefixOf' calls — no per-query 'Text.toLower' allocations.
module Hypha.Search.Fuzzy
  ( IndexedRow (..)
  , mkIndexedRow
  , displayRow
  , scoreRow
  , tokenize
  ) where

import Data.Text (Text)
import qualified Data.Text as Text

-- | A search-index row with both display fields (preserved as-is for
-- rendering) and precomputed lowercase fields for matching.
data IndexedRow = IndexedRow
  { irPkg   :: !Text
  , irMod   :: !Text
  , irName  :: !Text
  , irSig   :: !Text
  , irPkgL  :: !Text
  , irModL  :: !Text
  , irNameL :: !Text
  , irQualL :: !Text   -- ^ "<pkg>.<mod>.<name>" lowercased; used as the
                       --   haystack for dotted qualified queries.
  , irNameLen :: !Int  -- ^ Cached @Text.length irName@; used for the
                       --   shortest-name tie-breaker.
  }
  deriving stock (Show, Eq)

-- | Build an 'IndexedRow' from raw display fields.
mkIndexedRow :: Text -> Text -> Text -> Text -> IndexedRow
mkIndexedRow pkg modPath name sig =
  let pkgL  = Text.toLower pkg
      modL  = Text.toLower modPath
      nameL = Text.toLower name
      qualL = pkgL <> "." <> modL <> "." <> nameL
  in IndexedRow
       { irPkg     = pkg
       , irMod     = modPath
       , irName    = name
       , irSig     = sig
       , irPkgL    = pkgL
       , irModL    = modL
       , irNameL   = nameL
       , irQualL   = qualL
       , irNameLen = Text.length name
       }

-- | Recover the (pkg, module, name, signature) tuple expected by the
-- existing result-rendering code.
displayRow :: IndexedRow -> (Text, Text, Text, Text)
displayRow r = (irPkg r, irMod r, irName r, irSig r)

-- | Lower-case and split a query into tokens.  Empty input yields @[]@.
tokenize :: Text -> [Text]
tokenize = filter (not . Text.null) . Text.words . Text.toLower

-- | Score an indexed row against a list of tokens.
--
-- Returns 'Nothing' when any token fails to match anywhere on the row.
-- Otherwise returns a non-negative integer where larger means a better
-- match.  Sort rows by descending score.
scoreRow :: [Text] -> IndexedRow -> Maybe Int
scoreRow []     _ = Nothing
scoreRow tokens r = do
  let go !acc []     = Just acc
      go !acc (t:ts) = case tokenScore r t of
        Nothing -> Nothing
        Just s  -> go (acc + s) ts
  base <- go 0 tokens
  pure (base + nameBonus (irNameLen r))

-- | Score one token against the row.  'Nothing' means the token didn't
-- match the row at all.
tokenScore :: IndexedRow -> Text -> Maybe Int
tokenScore r tok
  | Text.null tok                                = Just 0
  -- Symbol name wins by a wide margin.
  | tok == irNameL r                             = Just 1000
  | Text.isPrefixOf tok (irNameL r)              = Just 700
  | Text.isInfixOf  tok (irNameL r)              = Just 500
  -- Then the module path / qualified handle.
  | tok == irModL r                              = Just 400
  | Text.isPrefixOf tok (irModL r)               = Just 300
  | Text.isInfixOf  tok (irModL r)               = Just 220
  | Text.isInfixOf  tok (irQualL r)              = Just 200
  -- Then the package.
  | Text.isInfixOf  tok (irPkgL r)               = Just 150
  -- Last resort: subsequence anywhere (handles abbreviations like
  -- @lkpe@ \x2192 @lookupEnv@).
  | isSubsequence tok (irNameL r)                = Just 80
  | isSubsequence tok (irQualL r)                = Just 30
  | otherwise                                    = Nothing

-- | A length-based tie-breaker so @lookup@ outranks @lookupWithDefault@
-- when the user typed \"lookup\".
nameBonus :: Int -> Int
nameBonus nameLen = max 0 (60 - nameLen)

-- | Is @needle@ a (not necessarily contiguous) subsequence of @hay@?
isSubsequence :: Text -> Text -> Bool
isSubsequence needle hay = go (Text.unpack needle) (Text.unpack hay)
  where
    go []     _      = True
    go _      []     = False
    go (n:ns) (h:hs)
      | n == h    = go ns hs
      | otherwise = go (n:ns) hs
