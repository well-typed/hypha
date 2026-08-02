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

import           Data.Containers.ListUtils (nubOrd)
import           Data.List (sort)
import qualified Data.Set as Set
import qualified Data.Text as Text

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import Hypha.Search.Exports (emptyEnv)
import Hypha.Search.Fuzzy (Entity (..), IndexedRow (..))
import Hypha.Search.Index (DefinitionRef (..), IndexRow (..), Visibility (..))
import Hypha.Search.Indexer
  ( ComponentIndex (..), OutsideExport (..), componentScorerRows
  , indexComponentPure )
import Hypha.Source.Extensions (defaultLanguageSettings)
import Hypha.Types.ComponentName (ComponentKey (..))
import Hypha.Types.PackageId (PackageName (..))
import Hypha.Types.SymbolPath (ModulePath (..), Signature (..), SymbolName (..))
import Util.Fixture (depSources, fixtureSources, sourcesFor)
import Util.Row (envFromRows)

-- | The packages the fixture component may resolve a re-export through:
-- itself, and the neighbouring fixture package it re-exports from.
reexportDeps :: Set.Set PackageName
reexportDeps = Set.fromList [PackageName "reexport", PackageName "reexport-dep"]

-- | The fixture component with its dependency already indexed, which is
-- the state the real pass reaches by walking units dependencies-first.
fixture :: IO ComponentIndex
fixture = do
  srcs <- fixtureSources
  dep  <- depIndex
  pure (indexComponentPure (ComponentKey "reexport") reexportDeps
          (envFromRows (ciRows dep)) defaultLanguageSettings srcs)

-- | The dependency component, indexed on its own with nothing before it.
depIndex :: IO ComponentIndex
depIndex = do
  srcs <- depSources
  pure (indexComponentPure (ComponentKey "reexport-dep")
          (Set.singleton (PackageName "reexport-dep")) emptyEnv
          defaultLanguageSettings srcs)

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
      let ci = indexComponentPure (ComponentKey "reexport") reexportDeps emptyEnv
                 defaultLanguageSettings srcs
      ciNameMismatch ci
        @?= [(ModulePath "Fixture.Renamed", ModulePath "Fixture.Declared")]
      assertBool "rows use the declared-in-source name"
        (all ((== ModulePath "Fixture.Declared") . rowModule) (ciRows ci))

  , testCase "a re-exported symbol gets a row on the wrapper too" $ do
      ci <- fixture
      let mods = sort (map (unModulePath . rowModule) (rowsFor ci "insertBag"))
      mods @?= [ "Fixture.Facade", "Fixture.Internal", "Fixture.Strict"
               , "Fixture.StrictInternal", "Fixture.Wrapper" ]

  , testCase "each row's definition module is the real one" $ do
      ci <- fixture
      let defOf m =
            [ drModule (rowDefinition r)
            | r <- rowsFor ci "insertBag", rowModule r == ModulePath m ]
      defOf "Fixture.Wrapper"        @?= [ModulePath "Fixture.Internal"]
      -- Two hops from the definition, and still the definition.
      defOf "Fixture.Facade"         @?= [ModulePath "Fixture.Internal"]
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
      let ci = indexComponentPure (ComponentKey "reexport") reexportDeps emptyEnv
                 defaultLanguageSettings srcs
      ciRows ci @?= []
      length (ciParseFailures ci) @?= 1

  , testCase "a symbol nothing in the component exports gets no row" $ do
      -- Fixture.Internal uses 'length' and exports nothing of the sort, so
      -- it is not a re-export to resolve -- it is not an export at all.
      ci <- fixture
      rowsFor ci "length" @?= []

  , testCase "a symbol re-exported from another package gets a row" $ do
      ci <- fixture
      case [ r | r <- rowsFor ci "depThing"
               , rowModule r == ModulePath "Fixture.Imported" ] of
        [r] -> do
          rowDefinition r @?= DefinitionRef (ComponentKey "reexport-dep")
                                            (ModulePath "Dep.Internal")
          -- The signature comes from the dependency's parse, not from a
          -- name-keyed guess inside this component.
          rowSignature r  @?= Signature "depThing :: Int -> Int"
        other -> fail ("expected one depThing row, got " <> show (length other))

  , testCase "a two-hop cross-package re-export lands on the declaration" $ do
      -- Fixture.TwoHop -> Dep.Facade -> Dep.Internal, the base:Data.List
      -- shape.  The environment carries the dependency's own resolved
      -- definition, so the middle hop is skipped for free.
      ci <- fixture
      case [ r | r <- rowsFor ci "depThing"
               , rowModule r == ModulePath "Fixture.TwoHop" ] of
        [r] -> do
          rowDefinition r @?= DefinitionRef (ComponentKey "reexport-dep")
                                            (ModulePath "Dep.Internal")
          rowSignature r  @?= Signature "depThing :: Int -> Int"
        other -> fail ("expected one TwoHop row, got " <> show (length other))

  , testCase "a dependency symbol nobody re-exports gets no row here" $ do
      ci <- fixture
      rowsFor ci "depUnused" @?= []

  , testCase "an unresolvable cross-package export is reported, not dropped" $ do
      -- The same facade with an empty environment: the dependency has not
      -- been indexed, so there is no signature to give.  Silently producing
      -- nothing is what hid base for a whole release.
      srcs <- sourcesFor
        [ ("test/fixtures/reexport/src/Fixture/Imported.hs", "Fixture.Imported", Exposed) ]
      let ci = indexComponentPure (ComponentKey "reexport") reexportDeps emptyEnv
                 defaultLanguageSettings srcs
      ciRows ci @?= []
      ciUnresolved ci
        @?= [ OutsideExport
                { oeModule   = ModulePath "Fixture.Imported"
                , oeName     = SymbolName "depThing"
                , oeExpected = ModulePath "Dep.Internal"
                } ]

  , testCase "a module re-export we cannot expand is reported, not dropped" $ do
      -- Fixture.Reflect exports `module Data.List`, which is not part of
      -- this component.  Those names cannot be expanded, so they never
      -- enter the resolver's work list and never reach ciUnresolved
      -- either -- mtl's Control.Monad.State exports module Control.Monad
      -- and contributed none of its names, with nothing said.
      srcs <- sourcesFor
        [ ("test/fixtures/reexport/src/Fixture/Reflect.hs", "Fixture.Reflect", Exposed) ]
      let ci = indexComponentPure (ComponentKey "reexport") reexportDeps emptyEnv
                 defaultLanguageSettings srcs
      ciExternalModuleForms ci
        @?= [(ModulePath "Fixture.Reflect", ModulePath "Data.List")]

  , testCase "an exported class method gets no row, and is reported" $ do
      -- The parser reports top-level declarations only, so a class method
      -- is a name the component exports and declares nowhere (issue 043).
      -- It must not be silently absent: it resolves to DefinedOutside and
      -- travels out through ciUnresolved, naming the module that could not
      -- account for it.
      srcs <- sourcesFor
        [ ("test/fixtures/reexport/src/Fixture/Klass.hs", "Fixture.Klass", Exposed) ]
      let ci = indexComponentPure (ComponentKey "reexport") reexportDeps emptyEnv
                 defaultLanguageSettings srcs
      rowsFor ci "klassMethod" @?= []
      assertBool ("expected klassMethod in " <> show (ciUnresolved ci))
        (SymbolName "klassMethod" `elem` map oeName (ciUnresolved ci))

  , testCase "the package row is not emitted once per component" $ do
      -- componentScorerRows is per component; a package is not a
      -- per-component fact.  Emitting one here made a project whose own
      -- package has a library and two executables answer its own name
      -- three times, and collapseRows dedups symbols only.
      ci <- fixture
      let entities = map irEntity (componentScorerRows (ciRows ci))
      [ () | EntityPackage _ _ <- entities ] @?= []
      -- And one module row per module, not one per symbol in it.
      let mods = [ m | EntityModule _ m _ <- entities ]
      length mods @?= length (nubOrd mods)
  ]
