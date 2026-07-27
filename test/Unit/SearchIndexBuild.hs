{-# LANGUAGE OverloadedStrings #-}
-- | Coverage for row construction: which modules exist, what they are
-- called, and where each symbol is defined.
--
-- Every case corresponds to a row shape observed in a real cache and known
-- to be wrong: @data.map.strict.internal@ (lowercase, from a file path),
-- @compiler.GHC.Data.Word64Map.Internal@ (a source-dir segment inside the
-- module name), @concasync@ (a script mistaken for a module), and
-- @Data.IntMap.Lazy.insertWith :: … Map k a@ (a signature resolved by
-- symbol name).
module Unit.SearchIndexBuild (tests) where

import           Data.List (sort)
import qualified Data.Text as Text

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import Hypha.Search.Index (IndexRow (..), Visibility (..))
import Hypha.Search.Indexer
  ( ComponentIndex (..), indexComponentPure )
import Hypha.Source.Extensions (defaultLanguageSettings)
import Hypha.Types.ComponentName (ComponentKey (..))
import Hypha.Types.SymbolPath (ModulePath (..), Signature (..), SymbolName (..))
import Util.Fixture (fixtureSources, sourcesFor)

fixture :: IO ComponentIndex
fixture = do
  srcs <- fixtureSources
  pure (indexComponentPure (ComponentKey "reexport") defaultLanguageSettings srcs)

rowsFor :: ComponentIndex -> Text.Text -> [IndexRow]
rowsFor ci n = [ r | r <- ciRows ci, rowName r == SymbolName n ]

tests :: TestTree
tests = testGroup "Unit.SearchIndexBuild"
  [ testCase "every module name is the one its source declares" $ do
      ci <- fixture
      let mods = sort (map (unModulePath . rowModule) (ciRows ci))
      assertBool "no lowercase-only module name"
        (all (\m -> m /= Text.toLower m) mods)
      assertBool "no source-dir segment in a module name"
        (all (not . Text.isPrefixOf "src.") mods)
      assertBool "Fixture.Declared present (header beats path)"
        (ModulePath "Fixture.Declared" `elem` map rowModule (ciRows ci))

  , testCase "a path/header disagreement is reported, and the header wins" $ do
      srcs <- sourcesFor
        [ ("test/fixtures/reexport/src/Fixture/Renamed.hs", "Fixture.Renamed", Internal) ]
      let ci = indexComponentPure (ComponentKey "reexport") defaultLanguageSettings srcs
      ciNameMismatch ci
        @?= [(ModulePath "Fixture.Renamed", ModulePath "Fixture.Declared")]
      assertBool "rows use the declared-in-source name"
        (all ((== ModulePath "Fixture.Declared") . rowModule) (ciRows ci))

  , testCase "a re-exported symbol gets a row on the wrapper too" $ do
      ci <- fixture
      let mods = sort (map (unModulePath . rowModule) (rowsFor ci "insertBag"))
      mods @?= [ "Fixture.Internal", "Fixture.Strict"
               , "Fixture.StrictInternal", "Fixture.Wrapper" ]

  , testCase "each row's definition module is the real one" $ do
      ci <- fixture
      let defOf m =
            [ rowDefModule r
            | r <- rowsFor ci "insertBag", rowModule r == ModulePath m ]
      defOf "Fixture.Wrapper"        @?= [ModulePath "Fixture.Internal"]
      defOf "Fixture.Strict"         @?= [ModulePath "Fixture.StrictInternal"]
      defOf "Fixture.Internal"       @?= [ModulePath "Fixture.Internal"]
      defOf "Fixture.StrictInternal" @?= [ModulePath "Fixture.StrictInternal"]

  , testCase "signatures come from the definition site, never by name" $ do
      -- The Data.IntMap.Lazy regression: two definitions share a name, and
      -- neither presentation may inherit the other one's signature.
      ci <- fixture
      let sigOf m =
            [ unSignature (rowSignature r)
            | r <- rowsFor ci "sizeBag", rowModule r == ModulePath m ]
      sigOf "Fixture.Other" @?= ["sizeBag :: [a] -> Int"]
      sigOf "Fixture.Internal" @?= ["sizeBag :: Bag a -> Int"]
      -- Fixture.Wrapper re-exports Fixture.Other's sizeBag, so it must
      -- carry the list signature, not the Bag one.
      sigOf "Fixture.Wrapper" @?= ["sizeBag :: [a] -> Int"]

  , testCase "visibility follows the cabal stanza" $ do
      ci <- fixture
      let visOf m = [ rowVisibility r | r <- ciRows ci, rowModule r == ModulePath m ]
      assertBool "StrictInternal rows are Internal"
        (all (== Internal) (visOf "Fixture.StrictInternal"))
      assertBool "Wrapper rows are Exposed"
        (all (== Exposed) (visOf "Fixture.Wrapper"))
      assertBool "there are rows for both"
        (not (null (visOf "Fixture.StrictInternal")) && not (null (visOf "Fixture.Wrapper")))

  , testCase "a module that cannot be parsed yields no invented rows" $ do
      -- The old indexer logged the failure and then let the re-export pass
      -- fabricate rows for the module anyway, with signatures borrowed
      -- from whatever else shared the name.
      srcs <- sourcesFor
        [ ("test/fixtures/reexport/src/Fixture/Broken.hs", "Fixture.Broken", Exposed) ]
      let ci = indexComponentPure (ComponentKey "reexport") defaultLanguageSettings srcs
      ciRows ci @?= []
      length (ciParseFailures ci) @?= 1

  , testCase "a symbol defined outside the component gets no row" $ do
      -- We have no signature for it and its definition belongs to another
      -- index entry; a row here would be an invention.
      ci <- fixture
      rowsFor ci "length" @?= []
  ]
