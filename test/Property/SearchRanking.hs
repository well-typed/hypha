{-# LANGUAGE OverloadedStrings #-}
-- | Ranking and collapse properties: an exact package or module name
-- outranks the symbols underneath it, and collapse never loses a
-- definition.
module Property.SearchRanking (tests) where

import qualified Data.Text as Text

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Falsify (testProperty)
import qualified Test.Falsify.Generator as Gen
import qualified Test.Falsify.Predicate as P
import qualified Test.Falsify.Range as Range
import Test.Falsify.Property (assert, gen)
import Test.Tasty.HUnit (testCase, (@?=))

import Hypha.Search.Collapse
  ( SearchResult (..), SymbolResult (..), collapseRows, rankRows )
import Hypha.Search.Fuzzy
  ( ResultKind (..), mkModuleRow, mkPackageRow, mkSymbolRow, tokenize )
import Hypha.Search.Index (IndexRow, rowModule, Visibility (..))
import Hypha.Types.ComponentName (ComponentKey (..))
import Hypha.Types.PackageId (PackageName (..), Version (..))
import Hypha.Types.SymbolPath (SymbolName (..))
import Util.Row (row, rowIn)

symbolRow :: Text.Text -> Text.Text -> Text.Text -> IndexRow
symbolRow pkg modPath name = row pkg modPath name (name <> " :: Int")

firstKind :: [SearchResult] -> Maybe ResultKind
firstKind results = case results of
  (ResultPackage _ _ : _)  -> Just KindPackage
  (ResultModule _ _ _ : _) -> Just KindModule
  (ResultSymbol _ : _)     -> Just KindSymbol
  []                       -> Nothing

-- | Rank and collapse exactly as the server does, over the symbol rows
-- plus the package and module rows they imply.
resultsFor :: Text.Text -> [IndexRow] -> [SearchResult]
resultsFor q rows = collapseRows (rankRows (tokenize q) scorable)
  where
    scorable =
      mkPackageRow (PackageName "containers") (Version "0.7")
        : [ mkModuleRow (ComponentKey "containers") m Exposed | m <- modules ]
        ++ map mkSymbolRow rows

    modules = foldr addOnce [] (map rowModule rows)
    addOnce m acc = if m `elem` acc then acc else m : acc

tests :: TestTree
tests = testGroup "Property.SearchRanking"
  [ testCase "query 'containers' puts the package first" $
      firstKind (resultsFor "containers"
        [ symbolRow "containers" "Data.Map.Strict" "insertWith"
        , symbolRow "containers" "Data.Map.Strict" "containers"
        ]) @?= Just KindPackage

  , testCase "query 'Data.Map.Strict' puts the module first" $
      firstKind (resultsFor "Data.Map.Strict"
        [ symbolRow "containers" "Data.Map.Strict" "insertWith" ])
        @?= Just KindModule

  , testCase "a two-token query is a symbol query, not a module query" $
      -- 'Data.Map insertWith' names a module in its first token but is a
      -- symbol query; only a query that is nothing but an entity's name
      -- asks for the entity.
      firstKind (resultsFor "Data.Map.Strict insertWith"
        [ symbolRow "containers" "Data.Map.Strict" "insertWith" ])
        @?= Just KindSymbol

  , testProperty "an exact package name outranks any number of its symbols" $ do
      n <- gen (Gen.inRange (Range.between (1, 40)))
      let syms = [ symbolRow "containers" "Some.Module"
                     ("containers" <> Text.pack (show i))
                 | i <- [1 .. n :: Int] ]
          got  = firstKind (resultsFor "containers" syms)
      assert (P.eq P..$ ("expected", Just KindPackage)
                   P..$ ("actual", got))

  , testProperty "collapse keeps one result per definition, and loses none" $ do
      n <- gen (Gen.inRange (Range.between (1, 20)))
      let defs = [ "sym" <> Text.pack (show i) | i <- [1 .. n :: Int] ]
          rows = concat
            [ [ rowIn "containers" "Data.Map" s "" "Data.Map.Internal" Exposed
              , rowIn "containers" "Data.Map.Internal" s "" "Data.Map.Internal" Exposed
              ]
            | s <- defs
            ]
          names =
            [ unSymbolName (srName r) | ResultSymbol r <- collapseRows (map mkSymbolRow rows) ]
      assert (P.eq P..$ ("expected", defs) P..$ ("actual", names))
  ]
