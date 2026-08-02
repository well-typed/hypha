{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
-- | Resolve each module's exported names to the module that actually
-- declares them.
--
-- The predecessor of this module was a @Map SymbolName Signature@ built
-- from every local declaration in the component.  Such a map cannot
-- represent two definitions of one name, so it published
-- @Data.IntMap.Lazy.insertWith@ with @Data.Map.insertWith@'s signature —
-- an @IntMap@ function documented as operating on a @Map@.  Resolution
-- has to be per @(module, name)@ pair and has to follow imports, or it is
-- guessing.
--
-- The definition site it produces is load-bearing three times over: it
-- gives the index a signature read from the right module, it gives search
-- a sound key for collapsing an @.Internal@ row into the wrapper that
-- documents it, and it gives the module page the entries a pure re-export
-- module would otherwise not have.
module Hypha.Search.Reexport
  ( DefinitionSite (..)
  , Ambiguity (..)
  , Resolution (..)
  , resolveComponent
  , expandedExportNames
  , expandedExportNamesIn
  , externalModuleForms
  , definitionModule
  , sharedSegments
  ) where

import Data.Foldable qualified as Foldable
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text

import Hypha.Source.Interface
  ( ExportItem (..), ImportItem (..), ModuleInterface (..) )
import Hypha.Source.Interface qualified as Interface
import Hypha.Types.SymbolPath (ModulePath (..), SymbolName)

-- | Where an exported name is declared.
data DefinitionSite
  = DefinedHere
  | DefinedIn !ModulePath
    -- ^ Declared by another module of the same component.
  | DefinedOutside !ModulePath
    -- ^ No module of this component declares it; the field names the
    -- import we believe supplies it.  The indexer writes no row for
    -- these — we have no signature to give, and the definition belongs to
    -- another index entry.
  deriving stock (Show, Eq)

-- | Why a definition site was chosen, when more than one could have been.
data Ambiguity
  = Unambiguous
  | ResolvedAmongst !(NonEmpty ModulePath)
    -- ^ The candidates we rejected, kept so the choice is testable and
    -- reportable instead of an accident of list order.
  deriving stock (Show, Eq)

data Resolution = Resolution
  { resSite      :: !DefinitionSite
  , resAmbiguity :: !Ambiguity
  }
  deriving stock (Show, Eq)

-- | Fold 'DefinedHere' back to the asking module, so callers that only
-- want a 'ModulePath' need not case-split to get one.
definitionModule :: ModulePath -> DefinitionSite -> ModulePath
definitionModule asking = \case
  DefinedHere      -> asking
  DefinedIn m      -> m
  DefinedOutside m -> m

-- | Resolve every @(module, exported name)@ pair in a component.
--
-- Computed as a fixpoint from the declarations outwards rather than by
-- recursing from each name inwards.  The inward version was correct and
-- unusable: with no memoisation it re-derived the same sub-resolution once
-- per candidate per name, which on a real package (hundreds of modules,
-- most with unrestricted imports) burned minutes of CPU without producing
-- a row.  Bottom-up costs one pass per link in the longest re-export
-- chain, and chains are short.
--
-- Termination is structural: a round that resolves nothing stops the loop,
-- so a pair of modules re-exporting each other simply never resolves and
-- falls through to 'DefinedOutside'.  No visited set, no depth limit.
resolveComponent :: [ModuleInterface] -> Map (ModulePath, SymbolName) Resolution
resolveComponent ifaces = fixpoint seeded
  where
    byName :: Map ModulePath ModuleInterface
    byName = interfacesByName ifaces

    -- Every pair we owe an answer for.
    wanted =
      [ (i, n) | i <- ifaces, n <- expandedExportNamesIn byName (miName i) ]

    declaredSet :: Map ModulePath (Set SymbolName)
    declaredSet =
      Map.fromList [ (miName i, Set.fromList (Interface.declaredNames i)) | i <- ifaces ]

    declaresIn m n = case Map.lookup m declaredSet of
      Just ns -> n `Set.member` ns
      Nothing -> False

    -- Round zero: everything a module declares itself.
    seeded = Map.fromList
      [ ((miName i, n), Resolution DefinedHere Unambiguous)
      | (i, n) <- wanted
      , declaresIn (miName i) n
      ]

    fixpoint acc =
      let acc' = Foldable.foldl' step acc wanted
      in if Map.size acc' == Map.size acc then finish acc else fixpoint acc'

    step acc (i, n)
      | Map.member (miName i, n) acc = acc
      | otherwise = case rankedCandidates acc i n of
          []                  -> acc
          (winner : rejected) -> Map.insert (miName i, n)
            (Resolution (throughTo acc winner n)
                        (maybe Unambiguous ResolvedAmongst (NE.nonEmpty rejected)))
            acc

    -- A candidate qualifies once we know it can supply the name: it
    -- declares it, or an earlier round resolved it there.
    rankedCandidates acc i n =
      let (preferred, open) = candidates i n
      in case viable acc n preferred of
           [] -> rank i (viable acc n open)
           ps -> rank i ps

    viable acc n ms =
      [ m
      | m <- ms
      , Map.member m byName
      , declaresIn m n || resolvedInside (Map.lookup (m, n) acc)
      ]

    resolvedInside r = case r of
      Just (Resolution DefinedHere _)  -> True
      Just (Resolution (DefinedIn _) _) -> True
      _                                 -> False

    -- Follow the chain to the module that actually declares the name.
    -- Stopping at the first hop names a module that only passes the symbol
    -- along: @Data.Map@ re-exports @Data.Map.Lazy@, which re-exports
    -- @Data.Map.Internal@, and only the last of those has a declaration to
    -- read a signature or a source line from.
    throughTo acc winner n
      | declaresIn winner n = DefinedIn winner
      | otherwise = case Map.lookup (winner, n) acc of
          Just (Resolution (DefinedIn m) _) -> DefinedIn m
          _                                 -> DefinedIn winner

    -- Nothing inside the component supplies it: name the first import that
    -- plausibly does, so the module page can still list the symbol and say
    -- where it came from.  A module with no such import resolves to itself,
    -- which keeps this total without an 'error' — and since
    -- 'DefinedOutside' rows are never indexed, that value cannot reach a
    -- search result.
    finish acc = Foldable.foldl' addOutside acc wanted
      where
        addOutside m (i, n)
          | Map.member (miName i, n) m = m
          | otherwise = Map.insert (miName i, n)
              (Resolution (outsideFor i n) Unambiguous) m

    outsideFor i n = case [ iiModule ii
                          | ii <- miImports i
                          , explicitlyLists ii n || openImport ii n
                          ] of
      (m : _) -> DefinedOutside m
      []      -> DefinedOutside (miName i)

    -- An explicit import list is a statement about where a name comes
    -- from; an unrestricted import is not.  So explicit candidates are
    -- considered first, and only if none of them pans out do we look at
    -- the open ones.
    candidates i n =
      ( [ iiModule ii | ii <- miImports i, explicitlyLists ii n ]
      , [ iiModule ii | ii <- miImports i, openImport ii n ]
          ++ [ m | Just items <- [miExports i], ExportModule m <- items ]
      )

    explicitlyLists ii n = case iiNames ii of
      Just (False, ns) -> n `elem` ns
      _                -> False

    openImport ii n = case iiNames ii of
      Nothing         -> True
      Just (True, ns) -> n `notElem` ns   -- a hiding list that does not hide it
      Just (False, _) -> False

    -- Siblings before strangers, then lexicographic, so the winner never
    -- depends on the order modules were handed to us.
    rank i =
      sortOn (\m -> (negate (sharedSegments (miName i) m), unModulePath m))

-- | Every name a module exports, with the @module M@ re-export form
-- expanded against the rest of the component.
--
-- Shared with the module-page pass, because a page that iterated the raw
-- export list would silently omit exactly the names a wrapper exists to
-- re-export: @module Data.Map.Strict.Internal@ contributes no names of its
-- own until it is expanded.
expandedExportNames :: [ModuleInterface] -> ModulePath -> [SymbolName]
expandedExportNames ifaces = expandedExportNamesIn (interfacesByName ifaces)

-- | 'expandedExportNames' against a map the caller already has.
--
-- 'resolveComponent' asks once per interface, and rebuilding the map on
-- each call made that quadratic in the size of the component.
expandedExportNamesIn
  :: Map ModulePath ModuleInterface -> ModulePath -> [SymbolName]
expandedExportNamesIn byName asking = case Map.lookup asking byName of
  Nothing -> []
  Just i  -> Interface.interfaceExportedNames i ++ moduleFormNames i
  where
    moduleFormNames i =
      [ n
      | m      <- moduleForms i
      , Just target <- [Map.lookup m byName]
      , n      <- Interface.interfaceExportedNames target
      ]

-- | Index modules by the name their source declares.
interfacesByName :: [ModuleInterface] -> Map ModulePath ModuleInterface
interfacesByName ifaces = Map.fromList [ (miName i, i) | i <- ifaces ]

-- | The @module M@ items of a module's export list.
moduleForms :: ModuleInterface -> [ModulePath]
moduleForms i = [ m | Just items <- [miExports i], ExportModule m <- items ]

-- | The @module M@ re-exports naming a module the component does not
-- have, as @(the re-exporting module, the module it names)@.
--
-- Those names cannot be expanded here, so they never enter the resolver's
-- work list and never reach the unresolved report either — @mtl@'s
-- @Control.Monad.State@ exports @module Control.Monad@ and contributed
-- none of its names, with nothing said.  Reported at module granularity,
-- which is all we honestly know: what an out-of-component module exports
-- is not a question this pass can answer.
externalModuleForms
  :: [ModuleInterface] -> [(ModulePath, ModulePath)]
externalModuleForms ifaces =
  [ (miName i, m)
  | i <- ifaces
  , m <- moduleForms i
  , not (Map.member m byName)
  ]
  where
    byName = interfacesByName ifaces

-- | How many leading dot-separated segments two module paths share.
--
-- @Data.Map.Strict@ and @Data.Map.Internal@ share two; @Data.Map.Strict@
-- and @Data.Set.Internal@ share one.  Its predecessor in
-- "Hypha.Source.Locate" compared /characters/ of a dotted module prefix
-- against a slashed file path, which made those two look nearly
-- identical — and so the package sweep for @Data.Map.Internal.balanceL@
-- answered with @Data/Set/Internal.hs@.
sharedSegments :: ModulePath -> ModulePath -> Int
sharedSegments a b =
  length (takeWhile id (zipWith (==) (segments a) (segments b)))
  where
    segments = Text.splitOn "." . unModulePath
