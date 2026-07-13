{-# LANGUAGE OverloadedStrings #-}
module Unit.Hackage (tests) where

import Data.Aeson (encode, object, (.=))
import Data.ByteString.Lazy qualified as LBS
import Data.Time (getCurrentTime, secondsToNominalDiffTime)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool)
import System.IO.Temp (withSystemTempDirectory)

import Hypha.Hackage.Api
  (HackageClient(..), HackageError(..), mkOfflineHackageClient, packageJsonUrl)
import Hypha.Hackage.Cache (mkCacheKey, insertCache)
import Hypha.Hackage.Types (CacheKind(..), CachedResponse(..))
import Hypha.Types.PackageId (PackageName(..), Version(..))

tests :: TestTree
tests = testGroup "Unit.Hackage"
  [ testCase "Offline mode returns OfflineCacheMiss for fetchPackageJson" $
      withSystemTempDirectory "hypha-hackage" $ \cacheDir -> do
        client <- mkOfflineHackageClient cacheDir
        let pkgName = PackageName "async"
        result <- fetchPackageJson client pkgName
        case result of
          Left (OfflineCacheMiss name) -> assertEqual "Package name matches" pkgName name
          Left other -> fail ("Expected OfflineCacheMiss, got: " ++ show other)
          Right _ -> fail "Expected Left, got Right"

  , testCase "Offline mode returns OfflineCacheMiss for fetchVersions" $
      withSystemTempDirectory "hypha-hackage" $ \cacheDir -> do
        client <- mkOfflineHackageClient cacheDir
        let pkgName = PackageName "async"
        result <- fetchVersions client pkgName
        case result of
          Left (OfflineCacheMiss name) -> assertEqual "Package name matches" pkgName name
          Left other -> fail ("Expected OfflineCacheMiss, got: " ++ show other)
          Right _ -> fail "Expected Left, got Right"

  , testCase "Offline mode with empty cache returns OfflineCacheMiss" $
      withSystemTempDirectory "hypha-hackage" $ \cacheDir -> do
        client <- mkOfflineHackageClient cacheDir
        let pkgName = PackageName "nonexistent-package-12345"
        result <- fetchPackageJson client pkgName
        assertBool "Should return Left" (either (const True) (const False) result)
        case result of
          Left (OfflineCacheMiss name) -> assertEqual "Package name matches" pkgName name
          Left _ -> pure () -- Any error is acceptable in offline mode
          Right _ -> fail "Expected Left in offline mode"

  , testCase "Offline fetchVersions reads back what online fetchPackageJson cached" $
      withSystemTempDirectory "hypha-hackage" $ \cacheDir -> do
        let pkgName = PackageName "rncryptor"
            body = LBS.toStrict (encode (object ["0.3.0.2" .= ("normal" :: String), "0.0.1.0" .= ("normal" :: String)]))
        key <- mkCacheKey (packageJsonUrl pkgName)
        now <- getCurrentTime
        insertCache cacheDir key CachedResponse
          { crEtag = Nothing
          , crLastModified = Nothing
          , crStoredAt = now
          , crBody = body
          , crKind = TtlMutable (secondsToNominalDiffTime 900)
          }
        client <- mkOfflineHackageClient cacheDir
        result <- fetchVersions client pkgName
        assertEqual "versions read back from the package.json cache entry"
          (Right [Version "0.3.0.2", Version "0.0.1.0"]) result
  ]
