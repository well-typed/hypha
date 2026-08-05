{-# LANGUAGE OverloadedStrings #-}
module Unit.Hackage (tests) where

import Data.Aeson (encode, object, (.=))
import Data.ByteString.Lazy qualified as LBS
import Data.Time (getCurrentTime, secondsToNominalDiffTime)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

import Hypha.Hackage.Api
  (HackageClient(..), HackageError(..), mkOfflineHackageClient, packageJsonUrl)
import Hypha.Hackage.Source (fetchAndExtractSource)
import Hypha.Hackage.Cache (mkCacheKey, insertCache)
import Hypha.Hackage.Types (CacheKind(..), CachedResponse(..))
import Hypha.Types.PackageId (PackageId(..), PackageName(..), Version(..))

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

  , testCase "Offline mode refuses to fetch a source tarball" $
      withSystemTempDirectory "hypha-hackage" $ \cacheDir -> do
        -- Measured before this existed: `hypha --offline --cache-dir <empty>
        -- source base/Data.List/sortOn` answered, having downloaded both
        -- base and ghc-internal from Hackage.  `fetchAndExtractSource` took
        -- a client, ignored it, and built its own Manager, so the flag could
        -- not be honoured however carefully the caller was written.
        client <- mkOfflineHackageClient cacheDir
        let pid = PackageId (PackageName "async") (Version "2.2.5")
        result <- fetchSourceTarball client pid
        case result of
          Left (OfflineCacheMiss name) ->
            assertEqual "names the package it would not fetch"
              (PackageName "async") name
          Left other -> fail ("Expected OfflineCacheMiss, got: " ++ show other)
          Right _ -> fail "an offline client must not return tarball bytes"

  , testCase "Extraction asks the client for the bytes, and nothing else" $
      withSystemTempDirectory "hypha-hackage" $ \destParent -> do
        -- The wiring, not the refusal: a stub client whose tarball arm is
        -- an error proves `fetchAndExtractSource` goes through the record.
        -- If it ever builds its own Manager again, this passes only while
        -- offline -- so the assertion is that the stub's own error comes
        -- back, verbatim.
        let stub = HackageClient
              { fetchPackageJson   = \n -> pure (Left (OfflineCacheMiss n))
              , fetchVersions      = \n -> pure (Left (OfflineCacheMiss n))
              , fetchSourceTarball = \_ -> pure (Left (HttpError 451))
              }
            pid = PackageId (PackageName "async") (Version "2.2.5")
        result <- fetchAndExtractSource stub pid (destParent </> "async-2.2.5")
        case result of
          Left (HttpError 451) -> pure ()
          other -> fail ("expected the client's own error, got: " ++ show other)
  ]
