{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
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
  ( Entity (..)
  , ResultKind (..)
  , entityKind
  , IndexedRow (..)
  , mkSymbolRow
  , mkPackageRow
  , mkModuleRow
  , entityRows
  , entityComponent
  , scopeRows
  , scoreRow
  , tokenize
  ) where

import Data.Containers.ListUtils (nubOrd)
import Data.Text (Text)
import qualified Data.Text as Text

import Hypha.Search.Index (IndexRow (..), Visibility (..))
import Hypha.Types.ComponentName (ComponentKey (..))
import Hypha.Types.PackageId (PackageName (..), Version (..))
import Hypha.Types.SymbolPath (ModulePath (..), SymbolName (..))

-- | What a search result is /about/.
--
-- A package result has no signature and a module result has no definition
-- site, so the payload is a sum rather than a record with fields that are
-- meaningless for two of its three shapes.
data Entity
  = EntityPackage !PackageName !Version
  | EntityModule  !ComponentKey !ModulePath !Visibility
  | EntitySymbol  !IndexRow
  deriving stock (Show, Eq)

-- | The scoring discriminant.  Derived from 'Entity' rather than stored
-- beside it, so the two cannot disagree.
data ResultKind = KindPackage | KindModule | KindSymbol
  deriving stock (Show, Eq, Ord)

entityKind :: Entity -> ResultKind
entityKind e = case e of
  EntityPackage{} -> KindPackage
  EntityModule{}  -> KindModule
  EntitySymbol{}  -> KindSymbol

-- | A search-index row: the typed payload plus the precomputed lowercase
-- fields used for matching.
--
-- The payload sits beside the match fields rather than being flattened
-- into 'Text': scoring touches only the lowercase fields (so the hot path
-- stays allocation-free), while rendering reads 'irEntity' directly
-- instead of re-wrapping 'Text' back into a 'ModulePath' at the edge.
data IndexedRow = IndexedRow
  { irEntity  :: !Entity
  , irPkgL    :: !Text
  , irModL    :: !Text
  , irNameL   :: !Text
  , irQualL   :: !Text   -- ^ "<pkg>.<mod>.<name>" lowercased; the haystack
                         --   for dotted qualified queries.
  , irNameLen :: !Int    -- ^ Cached name length for the shortest-name
                         --   tie-breaker.
  }
  deriving stock (Show, Eq)

-- | A symbol row.
mkSymbolRow :: IndexRow -> IndexedRow
mkSymbolRow r = indexedRow
  (EntitySymbol r)
  (unComponentKey (rowComponent r))
  (unModulePath (rowModule r))
  (unSymbolName (rowName r))

-- | A package row, so a query naming a package can land on the package.
mkPackageRow :: PackageName -> Version -> IndexedRow
mkPackageRow pkg ver =
  indexedRow (EntityPackage pkg ver) (unPackageName pkg) "" ""

-- | A module row, so a query naming a module can land on the module.
mkModuleRow :: ComponentKey -> ModulePath -> Visibility -> IndexedRow
mkModuleRow comp modPath vis = indexedRow
  (EntityModule comp modPath vis)
  (unComponentKey comp)
  (unModulePath modPath)
  ""

indexedRow :: Entity -> Text -> Text -> Text -> IndexedRow
indexedRow ent pkg modPath name =
  let pkgL  = Text.toLower pkg
      modL  = Text.toLower modPath
      nameL = Text.toLower name
  in IndexedRow
       { irEntity  = ent
       , irPkgL    = pkgL
       , irModL    = modL
       , irNameL   = nameL
       , irQualL   = pkgL <> "." <> modL <> "." <> nameL
       , irNameLen = Text.length name
       }

-- | The package and module rows a set of symbol rows implies.
--
-- Synthesised rather than stored: they are a projection of rows we already
-- have, and deriving them in one place is what stops the freshly-built
-- index and the hydrated-from-cache index from disagreeing about which
-- entities exist.  A module contributes one row however many of its
-- symbols do.
entityRows :: PackageName -> Version -> [IndexRow] -> [IndexedRow]
entityRows pkg ver rows =
  mkPackageRow pkg ver
    : [ mkModuleRow c m v
      | (c, m, v) <- nubOrd
          [ (rowComponent r, rowModule r, rowVisibility r) | r <- rows ]
      ]

-- | Which component a row belongs to — the package itself for a package
-- row, the owning component for a module or symbol row.
entityComponent :: Entity -> Text
entityComponent = \case
  EntityPackage p _  -> unPackageName p
  EntityModule c _ _ -> unComponentKey c
  EntitySymbol r     -> unComponentKey (rowComponent r)

-- | Restrict rows to one component.
--
-- Both 'Nothing' and an explicitly empty name mean "no scope".  The empty
-- case is not defensive padding: it arrives once the scope chip has just
-- been cleared, because the hidden input's now-empty value is still included
-- in the htmx request.
--
-- Applied to /rows/, deliberately, and never to collapsed results.  A
-- definition presented by both @base@ and @ghc-internal@ folds into a single
-- result carrying the winning presentation's component, so filtering
-- afterwards dropped it from the other package's view entirely — restricting
-- search to @base@ found no @mapAccumL@ at all.  Filtering first means each
-- scope collapses its own package's presentations and always sees them.
scopeRows :: Maybe Text -> [IndexedRow] -> [IndexedRow]
scopeRows mScope rows = case mScope of
  Just s | not (Text.null s) ->
    filter ((== s) . entityComponent . irEntity) rows
  _ -> rows

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
  pure (base + nameBonus (irNameLen r) + kindBonus tokens r + visibilityBonus r)

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

-- | Entity kinds outrank field scores outright: a query that names a
-- package wants the package, not one of its ten thousand symbols.  The
-- bonus exceeds any reachable accumulation of field scores, so this is a
-- tier rather than a nudge.
--
-- Single-token exactness is deliberate.  @Data.Map insertWith@ names a
-- module in its first token but is a symbol query; only a query that is
-- /nothing but/ an entity's name asks for that entity itself.
kindBonus :: [Text] -> IndexedRow -> Int
kindBonus tokens r = case entityKind (irEntity r) of
  KindPackage | [t] <- tokens, t == irPkgL r -> 100000
  KindModule  | [t] <- tokens, t == irModL r -> 50000
  _                                          -> 0

-- | A public presentation of a symbol never ties with an internal one.
-- Small, because it breaks ties rather than reordering tiers — collapse is
-- what actually folds the internal row away.
visibilityBonus :: IndexedRow -> Int
visibilityBonus r = case irEntity r of
  EntitySymbol row     -> vis (rowVisibility row)
  EntityModule _ _ v   -> vis v
  EntityPackage _ _    -> 0
  where
    vis Exposed  = 20
    vis Internal = 0

-- | Is @needle@ a (not necessarily contiguous) subsequence of @hay@?
isSubsequence :: Text -> Text -> Bool
isSubsequence needle hay = go (Text.unpack needle) (Text.unpack hay)
  where
    go []     _      = True
    go _      []     = False
    go (n:ns) (h:hs)
      | n == h    = go ns hs
      | otherwise = go (n:ns) hs
