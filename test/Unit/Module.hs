{-# LANGUAGE OverloadedStrings #-}
module Unit.Module (tests) where

import qualified Data.Text as Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Hypha.BuildEnv.Type (BuildEnv (..))
import Hypha.Source.Locate (listExportedSymbols, parseExports, SourceLocation (..), locateSymbolDefinition)
import Hypha.Types.PackageId (PackageId (..), PackageName (..), Version (..))
import qualified Data.Set as Set

-- | A mock BuildEnv that returns a fixture source path.
mockBuildEnv :: FilePath -> BuildEnv IO
mockBuildEnv srcDir = BuildEnv
  { discoverInstalledPackages = pure Set.empty
  , locatePackageSource       = \_ -> pure (Just srcDir)
  , locateHaddockHtml         = \_ -> pure Nothing
  , ghcVersion                = pure (Version "9.6.7")
  }

testPkg :: PackageId
testPkg = PackageId (PackageName "async") (Version "2.2.5")

tests :: TestTree
tests = testGroup "Module"
  [ testCase "parseExports extracts symbols from module header" $ do
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
      parseExports src @?= ["Async", "async", "wait", "cancel", "concurrently", "race"]

  , testCase "parseExports handles single-line exports" $ do
      let src = "module Foo (bar, baz) where"
      parseExports src @?= ["bar", "baz"]

  , testCase "parseExports returns empty for no exports" $ do
      let src = "module Foo where"
      parseExports src @?= []

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
            , locateHaddockHtml         = \_ -> pure Nothing
            , ghcVersion                = pure (Version "9.6.7")
            }
      exps <- listExportedSymbols env testPkg "Control.Concurrent.Async"
      exps @?= []

  , testCase "locateSymbolDefinition finds definition line" $ do
      let env = mockBuildEnv "test/fixtures/fake-cabal-store/ghc-9.6.7/async-2.2.5-abc123456789/share/async"
      loc <- locateSymbolDefinition env testPkg "Control.Concurrent.Async" "module"
      case loc of
        Nothing -> error "expected SourceLocation"
        Just sl -> do
          slPath sl @?= "test/fixtures/fake-cabal-store/ghc-9.6.7/async-2.2.5-abc123456789/share/async/Control/Concurrent/Async.hs"
          slLine sl @?= 1

  , testCase "locateSymbolDefinition returns Nothing for missing symbol" $ do
      let env = mockBuildEnv "test/fixtures/fake-cabal-store/ghc-9.6.7/async-2.2.5-abc123456789/share/async"
      loc <- locateSymbolDefinition env testPkg "Control.Concurrent.Async" "nonexistent"
      loc @?= Nothing
  ]
