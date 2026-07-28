{-# LANGUAGE OverloadedStrings #-}
-- | Coverage for locating a definition given a component's modules.
--
-- Resolution, not sweeping: a symbol the asking module re-exports is
-- followed to its declaration, including across a package boundary, and a
-- symbol whose defining source is out of reach is reported absent rather
-- than approximated by the first same-named binding elsewhere.
module Unit.SourceLocate (tests) where

import           Data.List (isInfixOf)
import qualified Data.Map.Strict as Map

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import qualified Hypha.Source.Locate as Locate
import           Hypha.Search.Index
                   ( DefinitionRef (..), ImportedDefinitions (..)
                   , ModuleSource (..), noImportedDefinitions )
import           Hypha.Source.Extensions (defaultLanguageSettings)
import           Hypha.Types.ComponentName (ComponentKey (..))
import           Hypha.Types.SymbolPath (ModulePath (..), SymbolName (..))
import           Util.Fixture (depSources, fixtureSources)

-- | What the server hands a browsing pass: the definition site the index
-- resolved for each name, plus the dependency modules those sites name.
depDefinitions :: [ModuleSource] -> ImportedDefinitions
depDefinitions dep = ImportedDefinitions
  { idSites = Map.fromList
      [ ( SymbolName "depThing"
        , DefinitionRef (ComponentKey "reexport-dep") (ModulePath "Dep.Internal")
        ) ]
  , idSources = Map.fromList
      [ (msDeclaredName d, (ComponentKey "reexport-dep", d)) | d <- dep ]
  }

tests :: TestTree
tests = testGroup "Unit.SourceLocate"
  [ testCase "an intra-component re-export resolves to its definition" $ do
      srcs <- fixtureSources
      mLd  <- Locate.locateDefinitionInComponent defaultLanguageSettings
                (ComponentKey "reexport") srcs noImportedDefinitions
                (ModulePath "Fixture.Wrapper") (SymbolName "insertBag")
      case mLd of
        Just ld -> do
          Locate.ldModule ld    @?= ModulePath "Fixture.Internal"
          Locate.ldComponent ld @?= ComponentKey "reexport"
        Nothing -> fail "expected to locate insertBag in Fixture.Internal"

  , testCase "a symbol defined in a dependency is located in that package" $ do
      srcs <- fixtureSources
      dep  <- depSources
      mLd <- Locate.locateDefinitionInComponent defaultLanguageSettings
               (ComponentKey "reexport") srcs (depDefinitions dep)
               (ModulePath "Fixture.Imported") (SymbolName "depThing")
      case mLd of
        Just ld -> do
          Locate.ldModule ld    @?= ModulePath "Dep.Internal"
          Locate.ldComponent ld @?= ComponentKey "reexport-dep"
          assertBool "points into the dependency's tree"
            ("reexport-dep" `isInfixOf` Locate.slPath (Locate.ldLocation ld))
        Nothing -> fail "expected to locate depThing in reexport-dep"

  , testCase "a two-hop re-export resolves through the dependency's own facade" $ do
      -- Fixture.TwoHop -> Dep.Facade -> Dep.Internal, the shape that made
      -- /pkg/base/Data.List/mapAccumL say "symbol not found": the immediate
      -- import only passes the symbol along, so scanning it finds nothing.
      -- The definition site is supplied directly, as the index resolved it.
      srcs <- fixtureSources
      dep  <- depSources
      mLd <- Locate.locateDefinitionInComponent defaultLanguageSettings
               (ComponentKey "reexport") srcs (depDefinitions dep)
               (ModulePath "Fixture.TwoHop") (SymbolName "depThing")
      case mLd of
        Just ld -> do
          Locate.ldModule ld    @?= ModulePath "Dep.Internal"
          Locate.ldComponent ld @?= ComponentKey "reexport-dep"
        Nothing -> fail "expected to locate depThing through the two-hop chain"

  , testCase "a symbol whose dependency source is absent is not guessed at" $ do
      srcs <- fixtureSources
      mLd  <- Locate.locateDefinitionInComponent defaultLanguageSettings
                (ComponentKey "reexport") srcs noImportedDefinitions
                (ModulePath "Fixture.Imported") (SymbolName "depThing")
      mLd @?= Nothing
  ]
