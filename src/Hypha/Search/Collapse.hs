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
  , rankRows
  , collapseRows
  , resultHref
  , definitionHref
  , resultComponent
  ) where

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

data SymbolResult = SymbolResult
  { srComponent  :: !ComponentKey
  , srModule     :: !ModulePath
    -- ^ The presentation the user lands on: the most public module that
    -- exposes this definition.
  , srName       :: !SymbolName
  , srSignature  :: !Signature
  , srDefinition :: !DefinitionRef
  , srAlternates :: !Int
    -- ^ How many other presentations were folded in.  Rendered as a small
    -- affordance linking the definition site, so nothing is hidden.
  }
  deriving stock (Show, Eq)

-- | Score and order rows, best first.
rankRows :: [Text] -> [IndexedRow] -> [IndexedRow]
rankRows tokens rows =
  map snd (sortOn (Down . fst) [ (s, r) | r <- rows, Just s <- [scoreRow tokens r] ])

-- | Fold every presentation of one definition into a single result.
--
-- The group key is @(component, definition module, name)@ — not the name,
-- and not the name plus signature.  @Data.Map.Strict.insertWith@ and
-- @Data.Map.Lazy.insertWith@ have the same name /and/ the same signature
-- and are different functions; they differ only in where they are defined,
-- which is why that is the key.
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
                   winner = pickPresentation group
               in ResultSymbol (symbolResult winner (length group - 1))
                    : go (Map.insert k () seen) rest

    symbolOf r = case irEntity r of
      EntitySymbol row -> Just row
      _                -> Nothing

    entityResult = \case
      EntityPackage p v    -> ResultPackage p v
      EntityModule c m v   -> ResultModule c m v
      EntitySymbol row     -> ResultSymbol (symbolResult row 0)

symbolKey :: IndexRow -> (ComponentKey, DefinitionRef, SymbolName)
symbolKey r = (rowComponent r, rowDefinition r, rowName r)

symbolResult :: IndexRow -> Int -> SymbolResult
symbolResult r alternates = SymbolResult
  { srComponent  = rowComponent r
  , srModule     = rowModule r
  , srName       = rowName r
  , srSignature  = rowSignature r
  , srDefinition = rowDefinition r
  , srAlternates = alternates
  }

-- | The presentation of a definition the user should land on.
pickPresentation :: NonEmpty IndexRow -> IndexRow
pickPresentation = NE.head . NE.sortWith presentationRank

-- | Ordered: exposed before internal, then a path with no @Internal@
-- segment, then fewer segments, then lexicographic.  Total and
-- deterministic, so the winner does not depend on the order SQLite
-- happened to return rows in.
presentationRank :: IndexRow -> (Int, Int, Int, Text)
presentationRank row =
  ( case rowVisibility row of Exposed -> 0; Internal -> 1
  , if "Internal" `elem` segments then 1 else 0
  , length segments
  , unModulePath (rowModule row)
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

-- | The component a result belongs to, for the search bar's scope chip.
resultComponent :: SearchResult -> Text
resultComponent = \case
  ResultPackage p _  -> unPackageName p
  ResultModule c _ _ -> unComponentKey c
  ResultSymbol s     -> unComponentKey (srComponent s)
