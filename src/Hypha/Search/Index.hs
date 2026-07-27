{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
-- | The search index: what a row is, and what it means.
--
-- Kept free of any SQLite import so the dependency runs one way — the
-- cache layer knows about rows, rows know nothing about storage.
module Hypha.Search.Index
  ( Visibility (..)
  , visibilityToText
  , visibilityFromText
  , IndexRow (..)
  , currentIndexFormat
  ) where

import Data.Text (Text)

import Hypha.Types.ComponentName (ComponentKey)
import Hypha.Types.SymbolPath (ModulePath, Signature, SymbolName)

-- | Whether the module presenting a symbol is part of the component's
-- public surface (@exposed-modules@) or an implementation detail
-- (@other-modules@).
--
-- Search ranks 'Exposed' above 'Internal' so an internal presentation of
-- a symbol never outranks the wrapper that documents it.
data Visibility = Exposed | Internal
  deriving stock (Show, Eq, Ord)

visibilityToText :: Visibility -> Text
visibilityToText = \case
  Exposed  -> "exposed"
  Internal -> "internal"

-- | Parse a stored visibility.  'Nothing' for anything else, so the
-- caller decides what an unrecognised value means rather than having a
-- default silently chosen for it.
visibilityFromText :: Text -> Maybe Visibility
visibilityFromText = \case
  "exposed"  -> Just Exposed
  "internal" -> Just Internal
  _          -> Nothing

-- | One search-index entry.
--
-- 'rowDefModule' is the module that actually declares the symbol: equal
-- to 'rowModule' for a local declaration, and the definition site for a
-- re-export.  Carrying it is what lets search collapse
-- @Data.Map.Strict.Internal.insertWith@ into @Data.Map.Strict.insertWith@
-- without also merging @Data.Map.Lazy.insertWith@ — same name, same
-- signature, different definition.
data IndexRow = IndexRow
  { rowComponent  :: !ComponentKey
  , rowModule     :: !ModulePath
  , rowName       :: !SymbolName
  , rowSignature  :: !Signature
  , rowDefModule  :: !ModulePath
  , rowVisibility :: !Visibility
  }
  deriving stock (Show, Eq, Ord)

-- | Bumped whenever a row's meaning changes.
--
-- Generation 1 rows are not migrated but discarded: their module names
-- may have come from file paths and their signatures may have been
-- resolved by symbol name, and neither defect is detectable per row.  The
-- only honest options are to re-index or to lie.
currentIndexFormat :: Int
currentIndexFormat = 2
