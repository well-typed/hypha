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
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.List (sort)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import Hypha.Search.Exports (emptyEnv)
import Hypha.Search.Fuzzy (Entity (..), IndexedRow (..))
import Hypha.Search.Index (DefinitionRef (..), IndexRow (..), Visibility (..))
import Hypha.Search.Indexer
  ( ComponentIndex (..), OutsideExport (..), componentScorerRows
  , indexComponentPure, repairUnresolved )
import Hypha.Source.Extensions (defaultLanguageSettings)
import Hypha.Source.Origins
  ( ModuleOrigins (..), OriginError (..), OriginOracle (..) )
import Hypha.Types.ComponentName (ComponentKey (..))
import Hypha.Types.PackageId (PackageId (..), PackageName (..), Version (..))
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

-- | The fixture whose export no import can account for, with its
-- dependency already indexed.
blindIndex :: IO ComponentIndex
blindIndex = do
  srcs <- sourcesFor
    [ ("test/fixtures/reexport/src/Fixture/Blind.hs", "Fixture.Blind", Exposed) ]
  dep  <- depIndex
  pure (indexComponentPure (ComponentKey "reexport") reexportDeps
          (envFromRows (ciRows dep)) defaultLanguageSettings srcs)

-- | Run the repair pass over an index, against the same environment the
-- pure pass had.
repairedWith :: OriginOracle IO -> ComponentIndex -> IO ComponentIndex
repairedWith oracle ci = do
  dep <- depIndex
  repairUnresolved oracle (ComponentKey "reexport")
    (PackageId (PackageName "reexport") (Version "0.1"))
    reexportDeps (envFromRows (ciRows dep)) ci

-- | An oracle that answers from a table, and refuses anything else --
-- rather than returning an empty export list, which would read as "this
-- module exports nothing" and quietly repair nothing.
stubOracle
  :: [(ModulePath, Either OriginError [(SymbolName, NonEmpty ModulePath)])]
  -> OriginOracle IO
stubOracle table = OriginOracle $ \_ m ->
  pure $ case lookup m table of
    Just (Right pairs) -> Right (ModuleOrigins (Map.fromList pairs))
    Just (Left e)      -> Left e
    Nothing            -> Left (OriginNotAnInterface (unModulePath m))

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
                { oeModule     = ModulePath "Fixture.Imported"
                , oeName       = SymbolName "depThing"
                , oeCandidates = [ModulePath "Dep.Internal"]
                , oeVisibility = Exposed
                } ]

  , testCase "an export resolves past an import that cannot supply it" $ do
      -- Fixture.Shadowed's first-ranked import has no depThing; the
      -- second one declares it.  Committing to the first candidate is
      -- what dropped every symbol base:Control.Concurrent re-exports,
      -- because its first import is Prelude.
      srcs <- sourcesFor
        [ ("test/fixtures/reexport/src/Fixture/Shadowed.hs", "Fixture.Shadowed", Exposed) ]
      dep <- depIndex
      let ci = indexComponentPure (ComponentKey "reexport") reexportDeps
                 (envFromRows (ciRows dep)) defaultLanguageSettings srcs
      case rowsFor ci "depThing" of
        [r] -> do
          rowDefinition r @?= DefinitionRef (ComponentKey "reexport-dep")
                                            (ModulePath "Dep.Internal")
          rowSignature r  @?= Signature "depThing :: Int -> Int"
        other -> fail ("expected one depThing row, got " <> show (length other))
      ciUnresolved ci @?= []

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

  , testCase "an exported class method gets a row on the declaring module" $ do
      -- Class methods were a name the component exported and declared
      -- nowhere (issue 043): the parser reported top-level declarations
      -- only, so the index had no row and the server could not search for
      -- e.g. foldMap.  Now the method is its own declaration, so it must
      -- get a row with the signature GHC attaches to it, and leave the
      -- unresolved report.
      srcs <- sourcesFor
        [ ("test/fixtures/reexport/src/Fixture/Klass.hs", "Fixture.Klass", Exposed) ]
      let ci = indexComponentPure (ComponentKey "reexport") reexportDeps emptyEnv
                 defaultLanguageSettings srcs
      case rowsFor ci "klassMethod" of
        [r] -> do
          rowDefinition r @?= DefinitionRef (ComponentKey "reexport")
                                           (ModulePath "Fixture.Klass")
          rowSignature r  @?= Signature "klassMethod :: a -> Int"
        other -> fail ("expected one klassMethod row, got " <> show (length other))
      assertBool "klassMethod leaves the unresolved report"
        (SymbolName "klassMethod" `notElem` map oeName (ciUnresolved ci))

  , testCase "constructors and record fields reached through (..) get rows" $ do
      -- Shape's members are exported only as @Shape (..)@, so nothing
      -- names them in the export list: the wildcard has to expand against
      -- the declarations.  A constructor carries no signature line of its
      -- own, and used to get an empty one -- a blank column in the search
      -- list and @sig: ""@ out of hypha lookup.
      srcs <- sourcesFor
        [ ("test/fixtures/reexport/src/Fixture/Klass.hs", "Fixture.Klass", Exposed) ]
      let ci = indexComponentPure (ComponentKey "reexport") reexportDeps emptyEnv
                 defaultLanguageSettings srcs
          sigOf n = map rowSignature (rowsFor ci n)
      -- Slicing is line-granular throughout this module, so a member
      -- carries whatever punctuation shares its line -- the same text the
      -- module page shows.  Trimming it would need column spans.
      sigOf "Circle" @?= [Signature "= Circle { radius :: Int }"]
      sigOf "Square" @?= [Signature "| Square Int"]
      sigOf "radius" @?= [Signature "{ radius :: Int"]
      sequence_
        [ assertBool (show n <> " leaves the unresolved report")
            (n `notElem` map oeName (ciUnresolved ci))
        | n <- map SymbolName ["Circle", "Square", "radius"]
        ]

  , testCase "an export no import explains is repaired from the interface" $ do
      -- Fixture.Blind's only import has no depThing, and the module that
      -- declares it is never named in the source.  Syntax has nothing
      -- left to try; GHC's own interface file says where it comes from.
      ci  <- blindIndex
      ci' <- repairedWith
               (stubOracle
                  [ ( ModulePath "Fixture.Blind"
                    , Right [(SymbolName "depThing", ModulePath "Dep.Internal" :| [])] ) ])
               ci
      map oeName (ciUnresolved ci) @?= [SymbolName "depThing"]
      case rowsFor ci' "depThing" of
        [r] -> do
          rowDefinition r @?= DefinitionRef (ComponentKey "reexport-dep")
                                            (ModulePath "Dep.Internal")
          -- The signature comes from the dependency's own row, not from
          -- the interface: a .hi carries no source text.
          rowSignature r  @?= Signature "depThing :: Int -> Int"
        other -> fail ("expected one repaired row, got " <> show (length other))
      ciUnresolved ci' @?= []

  , testCase "an interface we cannot read leaves the export unresolved" $ do
      -- A package that has not been built has no .hi.  The export stays
      -- in the unresolved report and the reason travels with it; a
      -- swallowed failure would read as "this module exports nothing".
      ci  <- blindIndex
      let missing = OriginIfaceMissing
                      (PackageId (PackageName "reexport") (Version "0.1"))
                      (ModulePath "Fixture.Blind") ["nowhere/Fixture/Blind.hi"]
      ci' <- repairedWith
               (stubOracle [(ModulePath "Fixture.Blind", Left missing)]) ci
      rowsFor ci' "depThing" @?= []
      map oeName (ciUnresolved ci') @?= [SymbolName "depThing"]
      ciOriginFailures ci' @?= [(ModulePath "Fixture.Blind", missing)]

  , testCase "an origin no dependency exports is not invented" $ do
      -- The interface names a module, and the index has no row for that
      -- (module, name) -- an unindexed class method, say.  There is no
      -- signature to give, so there is no row to write.
      ci  <- blindIndex
      ci' <- repairedWith
               (stubOracle
                  [ ( ModulePath "Fixture.Blind"
                    , Right [(SymbolName "depThing", ModulePath "Dep.Unindexed" :| [])] ) ])
               ci
      rowsFor ci' "depThing" @?= []
      map oeName (ciUnresolved ci') @?= [SymbolName "depThing"]

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
