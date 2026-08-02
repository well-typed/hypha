{-# LANGUAGE OverloadedStrings #-}
-- | Coverage for the cross-package export environment.
--
-- The dependency filter is the load-bearing part: two unrelated packages
-- can expose a module of the same name, and without the filter a facade in
-- one would resolve against the other.
module Unit.SearchExports (tests) where

import qualified Data.Set as Set

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Hypha.Search.Exports
  ( Export (..), ExportChoice (..), envFromRows, lookupExport )
import Hypha.Search.Index (DefinitionRef (..), IndexRow, Visibility (..))
import Hypha.Types.ComponentName (ComponentKey (..))
import Hypha.Types.PackageId (PackageName (..))
import Hypha.Types.SymbolPath (ModulePath (..), Signature (..), SymbolName (..))
import Util.Row (row, rowFrom)

-- | What ghc-internal contributes: mapAccumL declared where it is shown.
ghcInternalRows :: [IndexRow]
ghcInternalRows =
  [ row "ghc-internal" "GHC.Internal.Data.Traversable" "mapAccumL" "sigL" ]

deps :: Set.Set PackageName
deps = Set.fromList [PackageName "ghc-internal", PackageName "ghc-prim"]

tests :: TestTree
tests = testGroup "Unit.SearchExports"
  [ testCase "a dependency's export is found with its signature and definition" $
      case lookupExport deps (ModulePath "GHC.Internal.Data.Traversable")
             (SymbolName "mapAccumL") (envFromRows ghcInternalRows) of
        Just ch -> do
          exSignature (ecChosen ch) @?= Signature "sigL"
          exDefinition (ecChosen ch)
            @?= DefinitionRef (ComponentKey "ghc-internal")
                              (ModulePath "GHC.Internal.Data.Traversable")
          ecRejected ch @?= []
        Nothing -> fail "expected a hit"

  , testCase "a component that is not a dependency is invisible" $
      lookupExport (Set.fromList [PackageName "ghc-prim"])
        (ModulePath "GHC.Internal.Data.Traversable") (SymbolName "mapAccumL")
        (envFromRows ghcInternalRows)
        @?= Nothing

  , testCase "an export the dependency itself re-exported keeps its true definition" $
      -- ghc-internal's GHC.Internal.Data.List presents mapAccumL but
      -- GHC.Internal.Data.Traversable defines it.  A facade resolving
      -- through the List module must land on Traversable, not on List.
      case lookupExport deps (ModulePath "GHC.Internal.Data.List")
             (SymbolName "mapAccumL")
             (envFromRows
                [ rowFrom "ghc-internal" "GHC.Internal.Data.List" "mapAccumL" "sigL"
                    (DefinitionRef (ComponentKey "ghc-internal")
                                   (ModulePath "GHC.Internal.Data.Traversable"))
                    Exposed
                ]) of
        Just ch ->
          drModule (exDefinition (ecChosen ch))
            @?= ModulePath "GHC.Internal.Data.Traversable"
        Nothing -> fail "expected a hit"

  , testCase "two dependencies exposing the same module report the rejected one" $ do
      let env = envFromRows
            [ row "alpha" "Shared.Mod" "thing" "sigA"
            , row "beta"  "Shared.Mod" "thing" "sigB"
            ]
          both = Set.fromList [PackageName "alpha", PackageName "beta"]
      case lookupExport both (ModulePath "Shared.Mod") (SymbolName "thing") env of
        Just ch -> do
          -- Lexicographic on the component, so the winner does not depend
          -- on the order rows arrived in.
          drComponent (exDefinition (ecChosen ch)) @?= ComponentKey "alpha"
          ecRejected ch
            @?= [DefinitionRef (ComponentKey "beta") (ModulePath "Shared.Mod")]
        Nothing -> fail "expected a hit"

  , testCase "a facade over a facade resolves through the direct dependency" $ do
      -- The asking package depends on base and not on ghc-internal.  base
      -- presents Data.Foldable.foldl' whose definition, since GHC 9.10,
      -- lives in ghc-internal.  Filtering on the definition's package told
      -- the asker "you do not depend on ghc-internal" and dropped the row,
      -- which is every custom prelude on a modern GHC.  What the asker
      -- named is a module of base, and that is what the filter must test.
      let env = envFromRows
            [ rowFrom "base" "Data.Foldable" "foldl'" "sigF"
                (DefinitionRef (ComponentKey "ghc-internal")
                               (ModulePath "GHC.Internal.Data.Foldable"))
                Exposed
            ]
          onlyBase = Set.fromList [PackageName "base"]
      case lookupExport onlyBase (ModulePath "Data.Foldable")
             (SymbolName "foldl'") env of
        Just ch -> do
          exSignature (ecChosen ch) @?= Signature "sigF"
          -- The definition travels out untouched: widening the dependency
          -- set to its transitive closure would have resolved this too,
          -- and would have thrown the guard away.
          exDefinition (ecChosen ch)
            @?= DefinitionRef (ComponentKey "ghc-internal")
                              (ModulePath "GHC.Internal.Data.Foldable")
        Nothing -> fail "expected a hit through the direct dependency"

  , testCase "the guard still holds: an unrelated presenter is invisible" $ do
      -- The same shape, but the presenting package is not a dependency
      -- either.  Two unrelated packages exposing a module of the same name
      -- must not resolve against each other.
      let env = envFromRows
            [ rowFrom "stranger" "Data.Foldable" "foldl'" "sigF"
                (DefinitionRef (ComponentKey "ghc-internal")
                               (ModulePath "GHC.Internal.Data.Foldable"))
                Exposed
            ]
      lookupExport (Set.fromList [PackageName "base"])
        (ModulePath "Data.Foldable") (SymbolName "foldl'") env
        @?= Nothing

  , testCase "a name no dependency exports is a miss" $
      lookupExport deps (ModulePath "GHC.Internal.Data.Traversable")
        (SymbolName "notThere") (envFromRows ghcInternalRows)
        @?= Nothing
  ]
