{-# LANGUAGE OverloadedStrings #-}
module Unit.Module (tests) where

import qualified Data.Text as Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Hypha.BuildEnv.Type (BuildEnv (..))
import Hypha.Source.Extensions (defaultLanguageSettings)
import Hypha.Source.Locate (exportedNamesOf, listExportedSymbols, SourceLocation (..), locateSymbolDefinition)
import Hypha.Types.PackageId (PackageId (..), PackageName (..), Version (..))
import qualified Data.Set as Set

-- | A mock BuildEnv that returns a fixture source path.
mockBuildEnv :: FilePath -> BuildEnv IO
mockBuildEnv srcDir = BuildEnv
  { discoverInstalledPackages = pure Set.empty
  , locatePackageSource       = \_ -> pure (Just srcDir)
  , locateRepoTarball         = \_ -> pure Nothing
  , locateHaddockHtml         = \_ -> pure Nothing
  , ghcVersion                = pure (Version "9.6.7")
  }

testPkg :: PackageId
testPkg = PackageId (PackageName "async") (Version "2.2.5")

tests :: TestTree
tests = testGroup "Module"
  [ testCase "the export list is read from the parse tree" $ do
      let src = Text.unlines
            [ "module Control.Concurrent.Async"
            , "  ( Async"
            , "  , async"
            , "  , wait"
            , "  , cancel"
            , "  , concurrently"
            , "  , race"
            , "  ) where"
            ]
      ns <- exportedNamesOf defaultLanguageSettings "Async.hs" src
      ns @?= ["Async", "async", "wait", "cancel", "concurrently", "race"]

  , testCase "a single-line export list is read too" $ do
      ns <- exportedNamesOf defaultLanguageSettings "Foo.hs"
              "module Foo (bar, baz) where"
      ns @?= ["bar", "baz"]

  , testCase "no export list and nothing declared means no names" $ do
      ns <- exportedNamesOf defaultLanguageSettings "Foo.hs" "module Foo where"
      ns @?= []

  , testCase "a constructor wildcard is not mistaken for a bare name" $ do
      -- The header scraper this replaced could not tell @Map(..)@ from
      -- @Map@, so a type's constructors were lost from every export list
      -- that used the wildcard form.
      ns <- exportedNamesOf defaultLanguageSettings "Bag.hs"
              "module Bag (Bag(..), empty) where\ndata Bag a = Bag [a]\nempty :: Bag a\nempty = Bag []"
      ns @?= ["Bag", "empty"]

  , testCase "listExportedSymbols reads fixture file" $ do
      let env = mockBuildEnv "test/fixtures/fake-cabal-store/ghc-9.6.7/async-2.2.5-abc123456789/share/async"
      exps <- listExportedSymbols env testPkg "Control.Concurrent.Async"
      exps @?= ["Async", "async", "wait", "cancel", "concurrently", "race"]

  , testCase "listExportedSymbols returns empty for missing module" $ do
      let env = mockBuildEnv "test/fixtures/fake-cabal-store/ghc-9.6.7/async-2.2.5-abc123456789/share/async"
      exps <- listExportedSymbols env testPkg "NonExistent.Module"
      exps @?= []

  , testCase "listExportedSymbols returns empty for missing package" $ do
      let env = BuildEnv
            { discoverInstalledPackages = pure Set.empty
            , locatePackageSource       = \_ -> pure Nothing
            , locateRepoTarball         = \_ -> pure Nothing
            , locateHaddockHtml         = \_ -> pure Nothing
            , ghcVersion                = pure (Version "9.6.7")
            }
      exps <- listExportedSymbols env testPkg "Control.Concurrent.Async"
      exps @?= []

  , testCase "locateSymbolDefinition finds definition line" $ do
      let env = mockBuildEnv "test/fixtures/fake-cabal-store/ghc-9.6.7/async-2.2.5-abc123456789/share/async"
      -- 'concurrently' has both a signature and a definition in the
      -- fixture (lines 29 and 30 respectively); the parser-backed
      -- locator returns the definition line, matching the prior
      -- contract.  The previous test searched for the literal
      -- @module@ keyword, which the old line-grep matched as if it
      -- were a top-level binding — the proper parser correctly
      -- declines, so the symbol under test was changed.
      loc <- locateSymbolDefinition env testPkg "Control.Concurrent.Async" "concurrently"
      case loc of
        Nothing -> error "expected SourceLocation"
        Just sl -> do
          slPath sl @?= "test/fixtures/fake-cabal-store/ghc-9.6.7/async-2.2.5-abc123456789/share/async/Control/Concurrent/Async.hs"
          slLine sl @?= 30

  , testCase "locateSymbolDefinition returns Nothing for missing symbol" $ do
      let env = mockBuildEnv "test/fixtures/fake-cabal-store/ghc-9.6.7/async-2.2.5-abc123456789/share/async"
      loc <- locateSymbolDefinition env testPkg "Control.Concurrent.Async" "nonexistent"
      loc @?= Nothing
  ]
