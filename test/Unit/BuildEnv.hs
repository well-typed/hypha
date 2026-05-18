{-# LANGUAGE OverloadedStrings #-}
module Unit.BuildEnv (tests) where

import Data.List (isInfixOf, isSuffixOf)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import System.FilePath ((</>))

import Hypha.BuildEnv.Type (BuildEnv (..))
import Hypha.BuildEnv.Cabal (mkCabalBuildEnv)
import Hypha.BuildEnv.Mock (MockBuildEnv (..), emptyMock, mkMockBuildEnv)
import Hypha.Types.PackageId (PackageName (..), Version (..), PackageId (..))

tests :: TestTree
tests = testGroup "Unit.BuildEnv"
  [ testMockReturnsConfiguredPaths
  , testCabalFindsAsync
  , testCabalLocateHaddock
  , testCabalMissingPackage
  ]

testMockReturnsConfiguredPaths :: TestTree
testMockReturnsConfiguredPaths = testCase "Mock returns configured source paths" $ do
  let asyncId = PackageId (PackageName "async") (Version "2.2.5")
      mock = emptyMock
        { mockPackages = Map.fromList
            [ (asyncId, (Just "/src/async", Just "/doc/async/index.html"))
            ]
        , mockGhcVersion = Version "9.6.7"
        }
      env = mkMockBuildEnv mock
  -- Check discoverInstalledPackages
  pkgs <- discoverInstalledPackages env
  pkgs @?= Set.singleton asyncId
  -- Check locatePackageSource
  src <- locatePackageSource env asyncId
  src @?= Just "/src/async"
  -- Check locateHaddockHtml
  doc <- locateHaddockHtml env asyncId
  doc @?= Just "/doc/async/index.html"
  -- Check ghcVersion
  ver <- ghcVersion env
  ver @?= Version "9.6.7"

testCabalFindsAsync :: TestTree
testCabalFindsAsync = testCase "Cabal impl finds async-2.2.5 in fake store fixture" $ do
  let storeRoot = "test" </> "fixtures" </> "fake-cabal-store" </> "ghc-9.6.7"
  result <- mkCabalBuildEnv storeRoot
  case result of
    Left err -> error ("Expected Right, got: " ++ show err)
    Right env -> do
      -- Check that async is discovered
      pkgs <- discoverInstalledPackages env
      let asyncId = PackageId (PackageName "async") (Version "2.2.5")
      assertBool "async should be in discovered packages" (asyncId `Set.member` pkgs)
      -- Check GHC version
      ver <- ghcVersion env
      ver @?= Version "9.6.7"

testCabalLocateHaddock :: TestTree
testCabalLocateHaddock = testCase "Cabal locateHaddockHtml hits stub index.html" $ do
  let storeRoot = "test" </> "fixtures" </> "fake-cabal-store" </> "ghc-9.6.7"
  result <- mkCabalBuildEnv storeRoot
  case result of
    Left err -> error ("Expected Right, got: " ++ show err)
    Right env -> do
      let asyncId = PackageId (PackageName "async") (Version "2.2.5")
      haddock <- locateHaddockHtml env asyncId
      case haddock of
        Nothing -> error "Expected to find Haddock HTML"
        Just path -> do
          -- The path should end with the expected suffix
          assertBool "path should end with index.html" ("index.html" `isSuffixOf` path)
          assertBool "path should contain async-2.2.5" ("async-2.2.5" `isInfixOf` path)

testCabalMissingPackage :: TestTree
testCabalMissingPackage = testCase "Cabal impl returns Nothing for missing package" $ do
  let storeRoot = "test" </> "fixtures" </> "fake-cabal-store" </> "ghc-9.6.7"
  result <- mkCabalBuildEnv storeRoot
  case result of
    Left err -> error ("Expected Right, got: " ++ show err)
    Right env -> do
      let missingId = PackageId (PackageName "nonexistent") (Version "1.0.0")
      -- locatePackageSource should return Nothing for missing package
      src <- locatePackageSource env missingId
      src @?= Nothing
      -- locateHaddockHtml should return Nothing for missing package
      doc <- locateHaddockHtml env missingId
      doc @?= Nothing
