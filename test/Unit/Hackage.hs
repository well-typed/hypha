{-# LANGUAGE OverloadedStrings #-}
module Unit.Hackage (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool)

import Hypha.Hackage.Api (HackageClient(..), HackageError(..), mkOfflineHackageClient)
import Hypha.Types.PackageId (PackageName(..))

tests :: TestTree
tests = testGroup "Unit.Hackage"
  [ testCase "Offline mode returns OfflineCacheMiss for fetchPackageJson" $ do
      client <- mkOfflineHackageClient
      let pkgName = PackageName "async"
      result <- fetchPackageJson client pkgName
      case result of
        Left (OfflineCacheMiss name) -> assertEqual "Package name matches" pkgName name
        Left other -> fail ("Expected OfflineCacheMiss, got: " ++ show other)
        Right _ -> fail "Expected Left, got Right"

  , testCase "Offline mode returns OfflineCacheMiss for fetchVersions" $ do
      client <- mkOfflineHackageClient
      let pkgName = PackageName "async"
      result <- fetchVersions client pkgName
      case result of
        Left (OfflineCacheMiss name) -> assertEqual "Package name matches" pkgName name
        Left other -> fail ("Expected OfflineCacheMiss, got: " ++ show other)
        Right _ -> fail "Expected Left, got Right"

  , testCase "Offline mode with empty cache returns OfflineCacheMiss" $ do
      client <- mkOfflineHackageClient
      let pkgName = PackageName "nonexistent-package-12345"
      result <- fetchPackageJson client pkgName
      assertBool "Should return Left" (either (const True) (const False) result)
      case result of
        Left (OfflineCacheMiss name) -> assertEqual "Package name matches" pkgName name
        Left _ -> pure () -- Any error is acceptable in offline mode
        Right _ -> fail "Expected Left in offline mode"
  ]
