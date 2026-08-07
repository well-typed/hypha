{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
-- | The search index: what a row is, and what it means.
--
-- Kept free of any SQLite import so the dependency runs one way — the
-- cache layer knows about rows, rows know nothing about storage.
module Hypha.Search.Index
  ( Visibility (..)
  , ModuleSource (..)
  , visibilityToText
  , visibilityFromText
  , DefinitionRef (..)
  , ImportedDefinitions (..)
  , noImportedDefinitions
  , IndexRow (..)
  , currentIndexFormat
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
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

-- | One module's bytes, plus the two facts only the cabal stanza knows:
-- which name the stanza expected it to have, and whether the stanza
-- exposes it.
--
-- Lives beside the row types rather than with the indexer so
-- "Hypha.Source.Locate" can take one without importing the builder that
-- imports it.
data ModuleSource = ModuleSource
  { msDeclaredName :: !ModulePath
    -- ^ The name the cabal stanza (or, failing that, the file path)
    -- expected.  Used only to detect and report a disagreement — the
    -- parse tree is the authority on what a module is called.
  , msPath         :: !FilePath
  , msVisibility   :: !Visibility
  , msContent      :: !Text
  }
  deriving stock (Show, Eq)

-- | Where a symbol is declared: which component, and which of its
-- modules.
--
-- A module alone was never an identity — two packages can expose a module
-- of the same name — which is why this is a pair.  Carrying the component
-- is what lets search collapse @base:Data.Traversable.mapAccumL@ into the
-- same result as @ghc-internal:GHC.Internal.Data.Traversable.mapAccumL@
-- without also merging two unrelated packages that happen to agree on a
-- module name.
data DefinitionRef = DefinitionRef
  { drComponent :: !ComponentKey
  , drModule    :: !ModulePath
  }
  deriving stock (Show, Eq, Ord)

-- | What a browsing pass knows about other components: where each of the
-- asking module's exports is declared, and the sources of the modules that
-- answer names.
--
-- Both halves come from the index, which is the only place a /transitively/
-- resolved definition site exists.  Following the immediate import instead
-- is one hop, and a hop is not enough: @base@'s @Data.List@ reaches
-- @GHC.Internal.Data.List@, which declares nothing and passes @mapAccumL@
-- along from @GHC.Internal.Data.Traversable@.  Scanning the immediate import
-- found no declaration, so the symbol card answered \"symbol not found\" for
-- a symbol search had just offered.
--
-- 'idSites' can be missing a name the module exports — the index may still
-- be building, and class methods have no rows at all — so consumers fall
-- back to resolving within the component and must never treat a miss as
-- \"no such symbol\".
data ImportedDefinitions = ImportedDefinitions
  { idSites   :: !(Map SymbolName DefinitionRef)
  , idSources :: !(Map ModulePath (ComponentKey, ModuleSource))
  }
  deriving stock (Show, Eq)

-- | Nothing resolved from outside, and nothing reachable either.
noImportedDefinitions :: ImportedDefinitions
noImportedDefinitions = ImportedDefinitions Map.empty Map.empty

-- | One search-index entry.
--
-- 'rowDefinition' is where the symbol actually is: the row's own
-- component and module for a local declaration, another module of the
-- component for an intra-package re-export, and another /component/ for a
-- cross-package one.  Carrying it is what lets search collapse
-- @Data.Map.Strict.Internal.insertWith@ into @Data.Map.Strict.insertWith@
-- without also merging @Data.Map.Lazy.insertWith@ — same name, same
-- signature, different definition.
data IndexRow = IndexRow
  { rowComponent  :: !ComponentKey
  , rowModule     :: !ModulePath
  , rowName       :: !SymbolName
  , rowSignature  :: !Signature
  , rowDefinition :: !DefinitionRef
  , rowVisibility :: !Visibility
  }
  deriving stock (Show, Eq, Ord)

-- | Bumped whenever a row's meaning changes.
--
-- Generation 1 rows are not migrated but discarded: their module names
-- may have come from file paths and their signatures may have been
-- resolved by symbol name, and neither defect is detectable per row.
-- Generation 2 rows go the same way for the same reason: a stored
-- @def_mod@ cannot be attributed to a component after the fact.  The only
-- honest options are to re-index or to lie.
--
-- Generation 3 rows go too: their signatures were sliced from the source
-- span, so any per-argument Haddock comment inside that span was stored
-- as though it were part of the type (5.27% of rows on a real cache).  A
-- stored signature cannot be repaired after the fact either — telling a
-- comment from an operator like @-->@ needs the parse tree the row no
-- longer has — and without the bump every existing cache would keep
-- serving the mangled text forever.
--
-- Generation 4 rows go for the adjacent reason: they were built with an
-- empty CPP macro environment, so a module gated on
-- @__GLASGOW_HASKELL__@ or @MIN_VERSION_*@ was read from its oldest
-- branch.  The per-component fingerprint cannot notice that — the source
-- did not change, the macros did — so the generation is the only thing
-- that can force those rows to be rebuilt.
currentIndexFormat :: Int
currentIndexFormat = 5
