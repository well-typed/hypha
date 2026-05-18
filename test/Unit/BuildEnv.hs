{-# LANGUAGE OverloadedStrings #-}
module Unit.BuildEnv (tests) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import System.FilePath ((</>))

import Hypha.BuildEnv.Type (BuildEnv (..))
import Hypha.BuildEnv.Cabal (CabalStoreError (..), mkCabalBuildEnv)
import Hypha.BuildEnv.Mock (MockBuildEnv (..), emptyMock, mkMockBuildEnv)
import Hypha.Types.PackageId (PackageName (..), Version (..), PackageId (..))

tests :: TestTree
tests = testGroup "Unit.BuildEnv"
  [ testMockReturnsConfiguredPaths
  , testCabalFindsAsync
  , testCabalLocateHaddock
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

-- Helper: like Data.List.isSuffixOf
isSuffixOf :: String -> String -> Bool
isSuffixOf suffix str = suffix == drop (length str - length suffix) str

-- Helper: like Data.List.isInfixOf
isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)

-- Helper: like Data.List.isPrefixOf
isPrefixOf :: String -> String -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

-- Helper: like Data.List.tails
tails :: [a] -> [[a]]
tails [] = [[]]
tails xs@(_:xs') = xs : tails xs'
