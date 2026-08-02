{-# LANGUAGE OverloadedStrings #-}
-- | The /producer/ of 'ImportedDefinitions', end to end.
--
-- Every other test of cross-package browsing hands the consumer a
-- hand-written definition-site map, so the suite proved "locate works
-- given the right map" and never that the server builds one.  Two
-- browsing releases shipped green and broken on exactly that gap
-- (@ac201a9@, @627899f@); this closes item 1 of @issues\/todo\/044@.
--
-- So: real rows in a real 'PackageCache', through the real
-- 'importedSourcesFor', and the value it returns fed to the real
-- 'locateDefinitionInComponent'.  Both suites would still pass if
-- @importedSourcesFor@ returned 'mempty'; this one would not.
module Unit.ImportedSources (tests) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import Hypha.Command.Server (importedSourcesFor)
import Hypha.Error (HyphaError (..), NotFoundReason (..))
import Hypha.Package.Resolver (PackageResolver (..), ResolvedPackage (..))
import Hypha.Search.Index
  ( DefinitionRef (..), ImportedDefinitions (..), ModuleSource (..)
  , Visibility (..) )
import Hypha.Search.PackageCache
  ( CacheOrigin (..), openPackageCacheAt, writeCachedIndex )
import Hypha.Source.Extensions (defaultLanguageSettings)
import Hypha.Source.Locate (LocatedDefinition (..), locateDefinitionInComponent)
import Hypha.Types.BuildPlan (PackageOrigin (..))
import Hypha.Types.ComponentName (ComponentKey (..))
import Hypha.Types.PackageId (PackageId (..), PackageName (..), Version (..))
import Hypha.Types.SymbolPath (ModulePath (..), SymbolName (..))
import Util.Fixture (sourcesFor)
import Util.Row (rowFrom)

depPkg :: PackageId
depPkg = PackageId (PackageName "reexport-dep") (Version "0.1.0")

-- | Answers for @reexport-dep@ and nothing else, so a lookup for any
-- other package fails the way the real resolver fails.
stubResolver :: PackageResolver IO
stubResolver = PackageResolver
  { resolvePkg = \name ->
      pure $ if name == PackageName "reexport-dep"
        then Right ResolvedPackage
               { rpPkgId         = depPkg
               , rpIsOutsidePlan = False
               , rpIsLocal       = True
               , rpDepsCount     = 0
               , rpOrigin        = OriginLocal "test/fixtures/reexport-dep"
               }
        else Left (NotFound (NotFoundPackageInPlan name))
  , resolveSrc = \pid ->
      pure $ if pkgName pid == PackageName "reexport-dep"
        then Right ("test" </> "fixtures" </> "reexport-dep")
        else Left (NotFound (NotFoundPackageInPlan (pkgName pid)))
  , fetchVrs   = \_ -> pure (Right [])
  }

-- | The row the indexer writes for @Fixture.Imported.depThing@: presented
-- by @reexport@, defined in @reexport-dep:Dep.Internal@.
withCachedRow :: (ImportedDefinitions -> IO a) -> IO a
withCachedRow k = withSystemTempDirectory "hypha-imp" $ \tmp -> do
  cache <- openPackageCacheAt (tmp </> "global.db") Nothing
  writeCachedIndex cache OriginGlobal "reexport" "0.1.0"
    [ rowFrom "reexport" "Fixture.Imported" "depThing" "depThing :: Int -> Int"
        (DefinitionRef (ComponentKey "reexport-dep") (ModulePath "Dep.Internal"))
        Exposed
    ]
  k =<< importedSourcesFor cache stubResolver "reexport"
          (ModulePath "Fixture.Imported")

tests :: TestTree
tests = testGroup "Unit.ImportedSources"
  [ testCase "the cached row becomes a definition site" $
      withCachedRow $ \imported ->
        Map.lookup (SymbolName "depThing") (idSites imported)
          @?= Just (DefinitionRef (ComponentKey "reexport-dep")
                                  (ModulePath "Dep.Internal"))

  , testCase "the defining module's source is loaded from its own package" $
      withCachedRow $ \imported ->
        case Map.lookup (ModulePath "Dep.Internal") (idSources imported) of
          Nothing -> fail ("no source for Dep.Internal in "
                            <> show (Map.keys (idSources imported)))
          Just (comp, ms) -> do
            comp @?= ComponentKey "reexport-dep"
            msDeclaredName ms @?= ModulePath "Dep.Internal"
            assertBool "the file really was read, not stubbed"
              ("depThing ::" `Text.isInfixOf` msContent ms)

  , testCase "what the producer builds is what locate resolves through" $
      -- The whole point: this value comes from the cache and the
      -- resolver, not from a literal in the test.
      withCachedRow $ \imported -> do
        sources <- sourcesFor
          [ ( "test/fixtures/reexport/src/Fixture/Imported.hs"
            , "Fixture.Imported", Exposed ) ]
        mLd <- locateDefinitionInComponent defaultLanguageSettings
                 (ComponentKey "reexport") sources imported
                 (ModulePath "Fixture.Imported") (SymbolName "depThing")
        case mLd of
          Nothing -> fail "the definition was not located"
          Just ld -> do
            ldComponent ld @?= ComponentKey "reexport-dep"
            ldModule ld    @?= ModulePath "Dep.Internal"

  , testCase "a row defined in the asking component supplies no extra source" $
      -- importedSourcesFor exists for cross-component definitions only;
      -- an intra-component one needs no second parse.
      withSystemTempDirectory "hypha-imp" $ \tmp -> do
        cache <- openPackageCacheAt (tmp </> "global.db") Nothing
        writeCachedIndex cache OriginGlobal "reexport" "0.1.0"
          [ rowFrom "reexport" "Fixture.Wrapper" "insertBag" "sig"
              (DefinitionRef (ComponentKey "reexport")
                             (ModulePath "Fixture.Internal"))
              Exposed
          ]
        imported <- importedSourcesFor cache stubResolver "reexport"
                      (ModulePath "Fixture.Wrapper")
        idSites imported   @?= Map.empty
        idSources imported @?= Map.empty
  ]
