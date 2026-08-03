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
  , resolveComponent
  , ComponentExports (..)
  , componentExports
  , expandedExportNames
  , expandedExportNamesIn
  , externalModuleForms
  , definitionModule
  , sharedSegments
  ) where

import Data.Containers.ListUtils (nubOrd)
import Data.Foldable qualified as Foldable
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text

import Hypha.Source.Interface
  ( ExportItem (..), ImportItem (..), ModuleInterface (..) )
import Hypha.Source.Interface qualified as Interface
import Hypha.Source.Parser qualified as Parser
import Hypha.Types.SymbolPath (ModulePath (..), SymbolName (..))

-- | Where an exported name is declared.
data DefinitionSite
  = DefinedHere
  | DefinedIn !ModulePath
    -- ^ Declared by another module of the same component.
  | DefinedOutside !(NonEmpty ModulePath)
    -- ^ No module of this component declares it; the field ranks every
    -- import that could supply it, best first.  A /list/ because one
    -- import cannot be trusted: an open @import Prelude@ can
    -- syntactically supply any name, and @base@'s @Control.Concurrent@
    -- opens with one — so naming a single candidate lost every symbol
    -- that module re-exports.  The caller probes the candidates in order
    -- and keeps the first that really exports the name.
  | NoSupplier
    -- ^ No module of this component declares it, and no import could
    -- have supplied it either.  The common case is a name from outside
    -- the component: a module re-exports something the component does
    -- not contain.  (Class methods used to land here too, before the
    -- parser emitted them as declarations.)
  deriving stock (Show, Eq)

-- | Fold 'DefinedHere' back to the asking module, so callers that only
-- want a 'ModulePath' need not case-split to get one.
--
-- 'DefinedOutside' folds to its best candidate and 'NoSupplier' to the
-- asking module — both are the caller's last resort, not an answer, and
-- a caller that can do better should case-split rather than call this.
definitionModule :: ModulePath -> DefinitionSite -> ModulePath
definitionModule asking = \case
  DefinedHere       -> asking
  DefinedIn m       -> m
  DefinedOutside ms -> NE.head ms
  NoSupplier        -> asking

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
resolveComponent :: [ModuleInterface] -> Map (ModulePath, SymbolName) DefinitionSite
resolveComponent ifaces = fixpoint seeded
  where
    exports :: ComponentExports
    exports = componentExports ifaces

    byName :: Map ModulePath ModuleInterface
    byName = ceByName exports

    -- Every pair we owe an answer for.
    wanted =
      [ (i, n) | i <- ifaces, n <- expandedExportNamesIn exports (miName i) ]

    declaredSet :: Map ModulePath (Set SymbolName)
    declaredSet =
      Map.fromList [ (miName i, Set.fromList (Interface.declaredNames i)) | i <- ifaces ]

    declaresIn m n = case Map.lookup m declaredSet of
      Just ns -> n `Set.member` ns
      Nothing -> False

    -- Round zero: everything a module declares itself.
    seeded = Map.fromList
      [ ((miName i, n), DefinedHere)
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
          (winner : _) -> Map.insert (miName i, n) (throughTo acc winner n) acc

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
      Just DefinedHere   -> True
      Just (DefinedIn _) -> True
      _                  -> False

    -- Follow the chain to the module that actually declares the name.
    -- Stopping at the first hop names a module that only passes the symbol
    -- along: @Data.Map@ re-exports @Data.Map.Lazy@, which re-exports
    -- @Data.Map.Internal@, and only the last of those has a declaration to
    -- read a signature or a source line from.
    throughTo acc winner n
      | declaresIn winner n = DefinedIn winner
      | otherwise = case Map.lookup (winner, n) acc of
          Just (DefinedIn m) -> DefinedIn m
          _                  -> DefinedIn winner

    -- Nothing inside the component supplies it: rank every import that
    -- plausibly does and hand over all of them, so the caller can probe
    -- rather than commit.  Committing to the first is what dropped
    -- @base:Control.Concurrent.isCurrentThreadBound@ — that module opens
    -- with @import Prelude@, which plausibly supplies every name and
    -- actually supplies none of them.
    finish acc = Foldable.foldl' addOutside acc wanted
      where
        addOutside m (i, n)
          | Map.member (miName i, n) m = m
          | otherwise = Map.insert (miName i, n) (outsideFor i n) m

    outsideFor i n = case rankedImports i n of
      (m : ms) -> DefinedOutside (m :| ms)
      []       -> NoSupplier

    -- Ranked by the rule the in-component candidates already use, so the
    -- order is a function of the module rather than of the order its
    -- imports happen to be written in.
    rankedImports i n =
      let (preferred, open) = candidates i n
          explicit          = nubOrd preferred
      in rank i explicit
           ++ rank i [ m | m <- nubOrd open, m `notElem` explicit ]

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
expandedExportNames ifaces = expandedExportNamesIn (componentExports ifaces)

-- | 'expandedExportNames' against tables the caller already has.
--
-- 'resolveComponent' asks once per interface, and rebuilding them on
-- each call made that quadratic in the size of the component.
expandedExportNamesIn :: ComponentExports -> ModulePath -> [SymbolName]
expandedExportNamesIn ce asking = nubOrd $ case Map.lookup asking byName of
  Nothing -> []
  Just i  -> Interface.interfaceExportedNames i ++ moduleFormNames i
    ++ [ n
       | wc <- wildcardNames i
       , n  <- subordinatesOf ce wc
       ]
  where
    byName = ceByName ce

    wildcardNames i =
      [ wc | Just items <- [miExports i], ExportSymbolAll wc <- items ]

    moduleFormNames i =
      [ n
      | m      <- moduleForms i
      , Just target <- [Map.lookup m byName]
      , n      <- Interface.interfaceExportedNames target
      ]

-- | A component's modules indexed the two ways export expansion needs
-- them.  Both tables are built once per component: the subordinate one
-- used to be a scan of every module's declarations per @T(..)@ export,
-- which is quadratic in the component and got worse the moment the
-- parser started emitting methods, constructors and fields.
data ComponentExports = ComponentExports
  { ceByName       :: !(Map ModulePath ModuleInterface)
  , ceSubordinates :: !(Map SymbolName [SymbolName])
    -- ^ Container name to the names declared inside it.  Keyed by name
    -- alone, because the module that writes @T(..)@ is generally not the
    -- one that declares @T@ — that is the whole point of the form. Two
    -- unrelated same-named containers in one component therefore pool
    -- their members; the surplus names resolve nowhere and drop out.
  }

-- | Build the export tables for a component.
componentExports :: [ModuleInterface] -> ComponentExports
componentExports ifaces = ComponentExports
  { ceByName       = interfacesByName ifaces
  , ceSubordinates = Map.fromListWith (++)
      [ (SymbolName parent, [SymbolName (Parser.declName child)])
      | i      <- ifaces
      , child  <- miDecls i
      , Just parent <- [Parser.declParent child]
      , parent `Set.member` containers i
      ]
  }
  where
    -- A parent name only counts when this module really declares the
    -- container: 'declParent' is a name, and a name is not proof.
    containers i = Set.fromList
      [ Parser.declName d
      | d <- miDecls i
      , Parser.declKind d `elem`
          [ Parser.DkClass, Parser.DkData, Parser.DkNewtype ]
      ]

-- | The subordinate names a @T(..)@ export contributes: a class's
-- methods, a data type's constructors and record fields.  A wildcard
-- naming something no module of the component declares expands to
-- nothing — the subordinates live at the definition site, and that site
-- is out of reach.
subordinatesOf :: ComponentExports -> SymbolName -> [SymbolName]
subordinatesOf ce wc = Map.findWithDefault [] wc (ceSubordinates ce)

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
