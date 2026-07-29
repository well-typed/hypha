{-# LANGUAGE OverloadedStrings #-}
-- | Coverage for collapse: which presentation of a definition wins, and
-- which pairs must never be merged.
module Unit.SearchCollapse (tests) where

import Data.Text (Text)

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Hypha.Search.Collapse
  ( SearchResult (..), SymbolResult (..), collapseRows, definitionPresentation
  , presentationHref, presentationLabel, resultHref )
import Hypha.Search.Fuzzy (mkSymbolRow, scopeRows)
import Hypha.Search.Index (DefinitionRef (..), IndexRow, Visibility (..))
import Hypha.Types.ComponentName (ComponentKey (..))
import Hypha.Types.SymbolPath (ModulePath (..))
import Util.Row (rowFrom, rowIn)

-- | An @insertWith@ row: presented by one module, defined in another.
mapRow :: Text -> Text -> Visibility -> IndexRow
mapRow presented defined vis = rowIn
  "containers" presented "insertWith"
  "insertWith :: Ord k => (a -> a -> a) -> k -> a -> Map k a -> Map k a"
  defined vis

collapse :: [IndexRow] -> [SearchResult]
collapse = collapseRows . map mkSymbolRow

tests :: TestTree
tests = testGroup "Unit.SearchCollapse"
  [ testCase "the wrapper wins over its .Internal definition site" $
      case collapse [ mapRow "Data.Map.Strict.Internal" "Data.Map.Strict.Internal" Exposed
                    , mapRow "Data.Map.Strict"          "Data.Map.Strict.Internal" Exposed
                    ] of
        [ResultSymbol s] -> do
          srModule s     @?= ModulePath "Data.Map.Strict"
          drModule (srDefinition s) @?= ModulePath "Data.Map.Strict.Internal"
          length (srAlternates s) @?= 1
        other -> fail ("expected one collapsed result, got " <> show (length other))

  , testCase "strict and lazy stay two results (same name, same signature)" $ do
      -- Both are insertWith with identical signatures.  Collapsing by name
      -- — or by name and signature — would merge them.  They differ only in
      -- definition site, which is why that is the key.
      let results = collapse
            [ mapRow "Data.Map.Strict" "Data.Map.Strict.Internal" Exposed
            , mapRow "Data.Map.Lazy"   "Data.Map.Internal"        Exposed
            ]
      length results @?= 2

  , testCase "Exposed beats Internal when both present the definition" $
      case collapse [ mapRow "Data.Map.Hidden" "Data.Map.Internal" Internal
                    , mapRow "Data.Map"        "Data.Map.Internal" Exposed
                    ] of
        [ResultSymbol s] -> srModule s @?= ModulePath "Data.Map"
        _ -> fail "expected one collapsed result"

  , testCase "a shorter path breaks a tie between two exposed wrappers" $
      case collapse [ mapRow "Data.Map.Strict.Extra" "Data.Map.Internal" Exposed
                    , mapRow "Data.Map"              "Data.Map.Internal" Exposed
                    ] of
        [ResultSymbol s] -> srModule s @?= ModulePath "Data.Map"
        _ -> fail "expected one collapsed result"

  , testCase "a path without an Internal segment beats one with" $
      case collapse [ mapRow "Data.Map.Internal.Debug" "Data.Map.Internal" Exposed
                    , mapRow "Data.Map.Public.Facade"  "Data.Map.Internal" Exposed
                    ] of
        [ResultSymbol s] -> srModule s @?= ModulePath "Data.Map.Public.Facade"
        _ -> fail "expected one collapsed result"

  , testCase "hrefs point at the presentation, not the definition" $
      case collapse [mapRow "Data.Map.Strict" "Data.Map.Strict.Internal" Exposed] of
        [r] -> resultHref r @?= "/pkg/containers/Data.Map.Strict/insertWith"
        _   -> fail "expected one result"

  , testCase "collapse is order-independent in its winner" $ do
      -- Ranked input arrives in whatever order scoring produced; the
      -- winner must not depend on it.
      let a = mapRow "Data.Map.Strict.Internal" "Data.Map.Strict.Internal" Exposed
          b = mapRow "Data.Map.Strict"          "Data.Map.Strict.Internal" Exposed
          winner rows = case collapse rows of
            [ResultSymbol s] -> Just (srModule s)
            _                -> Nothing
      winner [a, b] @?= Just (ModulePath "Data.Map.Strict")
      winner [b, a] @?= Just (ModulePath "Data.Map.Strict")

  , testCase "base's presentation wins over ghc-internal's definition" $ do
      -- The issue in one case: mapAccumL is declared in ghc-internal and
      -- published by base.  One result, presented by base.
      let ghcInternal = ModulePath "GHC.Internal.Data.Traversable"
          def = DefinitionRef (ComponentKey "ghc-internal") ghcInternal
      case collapse
             [ rowFrom "ghc-internal" "GHC.Internal.Data.Traversable"
                 "mapAccumL" "sig" def Exposed
             , rowFrom "ghc-internal" "GHC.Internal.Data.List"
                 "mapAccumL" "sig" def Exposed
             , rowFrom "base" "Data.Traversable" "mapAccumL" "sig" def Exposed
             ] of
        [ResultSymbol s] -> do
          srComponent s  @?= ComponentKey "base"
          srModule s     @?= ModulePath "Data.Traversable"
          srDefinition s @?= def
          length (srAlternates s) @?= 2
          resultHref (ResultSymbol s) @?= "/pkg/base/Data.Traversable/mapAccumL"
        other -> fail ("expected one collapsed result, got " <> show (length other))

  , testCase "a collapsed result names every presentation it folded in" $ do
      -- A count alone was enough while alternates were modules of one
      -- package.  Across packages it cannot answer "which other packages
      -- expose this?", which is all the +N affordance is for.
      let def = DefinitionRef (ComponentKey "ghc-internal")
                              (ModulePath "GHC.Internal.Data.Traversable")
      case collapse
             [ rowFrom "base" "Data.Traversable" "mapAccumL" "sig" def Exposed
             , rowFrom "ghc-internal" "GHC.Internal.Data.List"
                 "mapAccumL" "sig" def Exposed
             , rowFrom "ghc-internal" "GHC.Internal.Data.Traversable"
                 "mapAccumL" "sig" def Exposed
             ] of
        [ResultSymbol s] ->
          map presentationLabel (srAlternates s)
            @?= [ "ghc-internal:GHC.Internal.Data.List"
                , "ghc-internal:GHC.Internal.Data.Traversable"
                ]
        other -> fail ("expected one collapsed result, got " <> show (length other))

  , testCase "a lone presentation folds in nothing" $
      case collapse [mapRow "Data.Map.Strict" "Data.Map.Strict.Internal" Exposed] of
        [ResultSymbol s] -> srAlternates s @?= []
        _ -> fail "expected one result"

  , testCase "the definition label names its package" $
      case collapse
             [ rowFrom "base" "Data.Traversable" "mapAccumL" "sig"
                 (DefinitionRef (ComponentKey "ghc-internal")
                                (ModulePath "GHC.Internal.Data.Traversable"))
                 Exposed
             ] of
        [ResultSymbol s] ->
          presentationLabel (definitionPresentation (srDefinition s))
            @?= "ghc-internal:GHC.Internal.Data.Traversable"
        _ -> fail "expected one result"

  , testCase "two packages that merely share a module name stay two results" $ do
      -- Same module name, same symbol, different definitions.  Collapsing
      -- these would claim one package's code is the other's.
      let results = collapse
            [ rowFrom "alpha" "Shared.Mod" "thing" "sig"
                (DefinitionRef (ComponentKey "alpha") (ModulePath "Shared.Mod")) Exposed
            , rowFrom "beta"  "Shared.Mod" "thing" "sig"
                (DefinitionRef (ComponentKey "beta")  (ModulePath "Shared.Mod")) Exposed
            ]
      length results @?= 2

  , testCase "the component breaks a tie between identical presentations" $ do
      -- Two packages presenting one definition under the same module name.
      -- Which wins does not matter; that it is the same one every run does.
      let def = DefinitionRef (ComponentKey "core") (ModulePath "Core.Internal")
          rows =
            [ rowFrom "zeta"  "Facade" "thing" "sig" def Exposed
            , rowFrom "alpha" "Facade" "thing" "sig" def Exposed
            ]
      case (collapse rows, collapse (reverse rows)) of
        ([ResultSymbol a], [ResultSymbol b]) -> do
          srComponent a @?= ComponentKey "alpha"
          srComponent b @?= ComponentKey "alpha"
        _ -> fail "expected one collapsed result from each order"

  , testCase "scoping to a package keeps a definition another package also presents" $ do
      -- The bug: mapAccumL is presented by base and by ghc-internal and
      -- collapses to one result whose component is the winner's.  Filtering
      -- collapsed results dropped it from the loser's package view
      -- entirely, so restricting search to base found nothing.  Scope has
      -- to be applied to rows, before they are folded together.
      let def = DefinitionRef (ComponentKey "ghc-internal")
                              (ModulePath "GHC.Internal.Data.Traversable")
          rows = [ rowFrom "base" "Data.List" "mapAccumL" "sig" def Exposed
                 , rowFrom "base" "Data.Traversable" "mapAccumL" "sig" def Exposed
                 , rowFrom "ghc-internal" "GHC.Internal.Data.Traversable"
                     "mapAccumL" "sig" def Exposed
                 ]
          scopedTo s = collapseRows (scopeRows s (map mkSymbolRow rows))
      case scopedTo (Just "base") of
        [ResultSymbol s] -> do
          srComponent s @?= ComponentKey "base"
          map presentationLabel (srAlternates s) @?= ["base:Data.Traversable"]
        other -> fail ("base scope: expected one result, got " <> show (length other))
      -- And the other side of the same coin.
      case scopedTo (Just "ghc-internal") of
        [ResultSymbol s] -> do
          srComponent s @?= ComponentKey "ghc-internal"
          srAlternates s @?= []
        other -> fail ("ghc-internal scope: expected one, got " <> show (length other))
      -- No scope still folds all three into one.
      length (scopedTo Nothing) @?= 1

  , testCase "an empty scope is no scope" $ do
      let rows = map mkSymbolRow
            [ mapRow "Data.Map.Strict" "Data.Map.Strict.Internal" Exposed ]
      length (collapseRows (scopeRows (Just "") rows)) @?= 1
      length (collapseRows (scopeRows Nothing rows))   @?= 1

  , testCase "scoping to a package with no rows yields nothing" $
      collapseRows (scopeRows (Just "nope")
        (map mkSymbolRow [mapRow "Data.Map" "Data.Map.Internal" Exposed]))
        @?= []

  , testCase "the definition link names the defining component, not the presenting one" $
      -- base presents mapAccumL; ghc-internal defines it.  Building the
      -- link from the presentation's component would point at a module
      -- base does not have.
      case collapse [ rowFrom "base" "Data.Traversable" "mapAccumL" "sig"
                        (DefinitionRef (ComponentKey "ghc-internal")
                                       (ModulePath "GHC.Internal.Data.Traversable"))
                        Exposed
                    ] of
        [ResultSymbol s] ->
          presentationHref (srName s)
              (definitionPresentation (srDefinition s))
            @?= "/pkg/ghc-internal/GHC.Internal.Data.Traversable/mapAccumL"
        _ -> fail "expected one result"
  ]
