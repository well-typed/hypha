{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
-- | Rank scored rows, then fold every presentation of one definition into
-- a single result.
--
-- The user asks for @insertWith@ and gets one row per module that exposes
-- it — @Data.Map.Strict@, @Data.Map.Strict.Internal@, and any other
-- wrapper in the chain — with no reason to prefer one.  Hackage shows the
-- module that documents the symbol, and so should we, without hiding the
-- definition site from anyone who wants it.
module Hypha.Search.Collapse
  ( SearchResult (..)
  , SymbolResult (..)
  , Presentation (..)
  , rankRows
  , collapseRows
  , resultHref
  , definitionHref
  , presentationHref
  , presentationLabel
  , definitionLabel
  , resultComponent
  ) where

import Data.Containers.ListUtils (nubOrd)
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as Text

import Hypha.Search.Fuzzy (Entity (..), IndexedRow (..), scoreRow)
import Hypha.Search.Index (DefinitionRef (..), IndexRow (..), Visibility (..))
import Hypha.Types.ComponentName (ComponentKey (..))
import Hypha.Types.PackageId (PackageName (..), Version (..))
import Hypha.Types.SymbolPath (ModulePath (..), Signature, SymbolName (..))

-- | One rendered search result.
data SearchResult
  = ResultPackage !PackageName !Version
  | ResultModule  !ComponentKey !ModulePath !Visibility
  | ResultSymbol  !SymbolResult
  deriving stock (Show, Eq)

-- | One module of one component that exposes a definition.
--
-- A group's members differ only in this, which is why it is what the
-- collapsed result carries.
data Presentation = Presentation
  { prComponent :: !ComponentKey
  , prModule    :: !ModulePath
  }
  deriving stock (Show, Eq, Ord)

data SymbolResult = SymbolResult
  { srComponent  :: !ComponentKey
  , srModule     :: !ModulePath
    -- ^ The presentation the user lands on: the most public module that
    -- exposes this definition.
  , srName       :: !SymbolName
  , srSignature  :: !Signature
  , srDefinition :: !DefinitionRef
  , srAlternates :: ![Presentation]
    -- ^ The other presentations folded into this result, best-first.
    --
    -- A count was enough while every alternate was a module of the same
    -- package: "+2, defined in Data.Map.Internal" told the whole story.
    -- Now that a group can span packages it cannot, because the question
    -- the affordance exists to answer became "which /other packages/ expose
    -- this?" — and a number cannot answer it.
  }
  deriving stock (Show, Eq)

-- | Score and order rows, best first.
rankRows :: [Text] -> [IndexedRow] -> [IndexedRow]
rankRows tokens rows =
  map snd (sortOn (Down . fst) [ (s, r) | r <- rows, Just s <- [scoreRow tokens r] ])

-- | Fold every presentation of one definition into a single result.
--
-- The group key is @(definition, name)@ — not the name, and not the name
-- plus signature.  @Data.Map.Strict.insertWith@ and
-- @Data.Map.Lazy.insertWith@ have the same name /and/ the same signature
-- and are different functions; they differ only in where they are defined,
-- which is why that is the key.
--
-- The /presenting/ component is deliberately not part of the key.
-- @base:Data.Traversable.mapAccumL@ and
-- @ghc-internal:GHC.Internal.Data.Traversable.mapAccumL@ are one function
-- published under two surfaces, and the author meant it to be consumed
-- from @base@.  'DefinitionRef' carries its own component, so two packages
-- that merely share a module name still key apart.
--
-- Input order (already ranked) is preserved: a group appears where its
-- first member appeared.
collapseRows :: [IndexedRow] -> [SearchResult]
collapseRows rows = go Map.empty rows
  where
    -- NonEmpty by construction: a group exists because a row created it,
    -- so 'pickPresentation' needs no partial head.
    grouped = Map.fromListWith (<>)
      [ (symbolKey r, r :| []) | Just r <- map symbolOf rows ]

    go _ [] = []
    go seen (r : rest) = case symbolOf r of
      Nothing  -> entityResult (irEntity r) : go seen rest
      Just row ->
        let k = symbolKey row
        in if k `Map.member` seen
             then go seen rest
             else
               let group  = Map.findWithDefault (row :| []) k grouped
                   ranked = NE.sortWith presentationRank group
                   winner = NE.head ranked
                   -- nubOrd because one presentation can reach the scorer
                   -- twice — a unit hydrated from cache and then rebuilt
                   -- prepends its rows again — and a count that double-counts
                   -- is exactly the kind of lie the list replaced.
                   others = nubOrd (map presentationOf (NE.tail ranked))
               in ResultSymbol (symbolResult winner others)
                    : go (Map.insert k () seen) rest

    symbolOf r = case irEntity r of
      EntitySymbol row -> Just row
      _                -> Nothing

    entityResult = \case
      EntityPackage p v    -> ResultPackage p v
      EntityModule c m v   -> ResultModule c m v
      EntitySymbol row     -> ResultSymbol (symbolResult row [])

symbolKey :: IndexRow -> (DefinitionRef, SymbolName)
symbolKey r = (rowDefinition r, rowName r)

symbolResult :: IndexRow -> [Presentation] -> SymbolResult
symbolResult r alternates = SymbolResult
  { srComponent  = rowComponent r
  , srModule     = rowModule r
  , srName       = rowName r
  , srSignature  = rowSignature r
  , srDefinition = rowDefinition r
  , srAlternates = alternates
  }

presentationOf :: IndexRow -> Presentation
presentationOf r = Presentation (rowComponent r) (rowModule r)

-- | Ordered: exposed before internal, then a path with no @Internal@
-- segment, then fewer segments, then lexicographic on the module, then on
-- the component.  Total and deterministic, so the winner does not depend
-- on the order SQLite happened to return rows in — and now that a group can
-- span components, the module alone is no longer a total order.
--
-- This ladder is what picks @base:Data.Traversable@ (two segments, no
-- @Internal@) over @ghc-internal:GHC.Internal.Data.Traversable@ (four
-- segments, one @Internal@).  It ranks by the shape of the published
-- surface, not by any notion of which package the project "meant" to
-- depend on, so a facade with shorter module names than the package it
-- wraps would win; the @+N@ affordance is the escape hatch when the shape
-- misleads.
presentationRank :: IndexRow -> (Int, Int, Int, Text, Text)
presentationRank row =
  ( case rowVisibility row of Exposed -> 0; Internal -> 1
  , if "Internal" `elem` segments then 1 else 0
  , length segments
  , unModulePath (rowModule row)
  , unComponentKey (rowComponent row)
  )
  where
    segments = Text.splitOn "." (unModulePath (rowModule row))

resultHref :: SearchResult -> Text
resultHref = \case
  ResultPackage p _  -> "/pkg/" <> unPackageName p
  ResultModule c m _ -> "/pkg/" <> unComponentKey c <> "/" <> unModulePath m
  ResultSymbol s     -> "/pkg/" <> unComponentKey (srComponent s)
                          <> "/" <> unModulePath (srModule s)
                          <> "/" <> unSymbolName (srName s)

-- | Where the @+N@ affordance points: the definition site, so the escape
-- hatch out of a collapsed group is one click.
--
-- The component comes from the definition, not from the presentation: a
-- re-export can cross a package boundary, and @\/pkg\/base\/GHC.Internal…@
-- is a module @base@ does not have.
definitionHref :: SymbolResult -> Text
definitionHref s =
  "/pkg/" <> unComponentKey (drComponent (srDefinition s))
    <> "/" <> unModulePath (drModule (srDefinition s))
    <> "/" <> unSymbolName (srName s)

-- | Where one folded-in presentation lives, so an alternate can be reached
-- and not merely counted.
presentationHref :: SymbolName -> Presentation -> Text
presentationHref name p =
  "/pkg/" <> unComponentKey (prComponent p)
    <> "/" <> unModulePath (prModule p)
    <> "/" <> unSymbolName name

-- | @component:module@ — the form the UI shows a folded-in presentation in.
--
-- Always qualified by the component, even when it matches the presentation
-- the user landed on: the whole point of the label is to say /where else/,
-- and dropping the package for same-package alternates would make the two
-- cases indistinguishable at a glance.
presentationLabel :: Presentation -> Text
presentationLabel p =
  unComponentKey (prComponent p) <> ":" <> unModulePath (prModule p)

-- | @component:module@ for a definition site.
definitionLabel :: DefinitionRef -> Text
definitionLabel d =
  unComponentKey (drComponent d) <> ":" <> unModulePath (drModule d)

-- | The component a result belongs to, for the search bar's scope chip.
resultComponent :: SearchResult -> Text
resultComponent = \case
  ResultPackage p _  -> unPackageName p
  ResultModule c _ _ -> unComponentKey c
  ResultSymbol s     -> unComponentKey (srComponent s)
