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
  , definitionModule
  , sharedSegments
  ) where

import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
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
resolveComponent :: [ModuleInterface] -> Map (ModulePath, SymbolName) Resolution
resolveComponent ifaces = Map.fromList
  [ ((miName i, n), resolve Set.empty i n)
  | i <- ifaces
  , n <- expandedExportNames ifaces (miName i)
  ]
  where
    byName :: Map ModulePath ModuleInterface
    byName = Map.fromList [ (miName i, i) | i <- ifaces ]

    declares i n = n `elem` Interface.declaredNames i

    -- @visiting@ is the set of modules already on the resolution stack.
    -- A candidate already on the stack is dropped rather than followed,
    -- so a pair of modules re-exporting each other terminates with
    -- 'DefinedOutside' instead of recursing forever.
    resolve visiting i n
      | declares i n = Resolution DefinedHere Unambiguous
      | otherwise =
          let visiting'         = Set.insert (miName i) visiting
              (preferred, open) = candidates i n
              ranked = case viable visiting' n preferred of
                []  -> rank i (viable visiting' n open)
                ps  -> rank i ps
          in case ranked of
               (winner : rejected) -> Resolution
                 (DefinedIn winner)
                 (maybe Unambiguous ResolvedAmongst (NE.nonEmpty rejected))
               [] -> Resolution (outsideFor i n) Unambiguous

    viable visiting n ms =
      [ m
      | m <- ms
      , not (m `Set.member` visiting)
      , Just target <- [Map.lookup m byName]
      , supplies visiting target n
      ]

    -- A candidate supplies the name if it declares it, or can itself
    -- resolve it to a declaration inside the component.
    supplies visiting target n =
      declares target n
        || case resSite (resolve visiting target n) of
             DefinedIn _ -> True
             _           -> False

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

    -- Nothing inside the component supplies it: name the first import
    -- that plausibly does, so the module page can still list the symbol
    -- and say where it came from.  A module with no such import resolves
    -- to itself, which keeps the function total without an 'error' — and
    -- since 'DefinedOutside' rows are never indexed, that value cannot
    -- reach a search result.
    outsideFor i n = case [ iiModule ii
                          | ii <- miImports i
                          , explicitlyLists ii n || openImport ii n
                          ] of
      (m : _) -> DefinedOutside m
      []      -> DefinedOutside (miName i)

-- | Every name a module exports, with the @module M@ re-export form
-- expanded against the rest of the component.
--
-- Shared with the module-page pass, because a page that iterated the raw
-- export list would silently omit exactly the names a wrapper exists to
-- re-export: @module Data.Map.Strict.Internal@ contributes no names of its
-- own until it is expanded.
expandedExportNames :: [ModuleInterface] -> ModulePath -> [SymbolName]
expandedExportNames ifaces asking = case Map.lookup asking byName of
  Nothing -> []
  Just i  -> Interface.interfaceExportedNames i ++ moduleFormNames i
  where
    byName = Map.fromList [ (miName i, i) | i <- ifaces ]

    moduleFormNames i =
      [ n
      | Just items <- [miExports i]
      , ExportModule m <- items
      , Just target <- [Map.lookup m byName]
      , n <- Interface.interfaceExportedNames target
      ]

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
