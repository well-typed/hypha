{-# LANGUAGE OverloadedStrings #-}
-- | Coverage for collapse: which presentation of a definition wins, and
-- which pairs must never be merged.
module Unit.SearchCollapse (tests) where

import Data.Text (Text)

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Hypha.Search.Collapse
  ( SearchResult (..), SymbolResult (..), collapseRows, definitionHref
  , resultHref )
import Hypha.Search.Fuzzy (mkSymbolRow)
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
          srAlternates s @?= 1
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
          definitionHref s
            @?= "/pkg/ghc-internal/GHC.Internal.Data.Traversable/mapAccumL"
        _ -> fail "expected one result"
  ]
