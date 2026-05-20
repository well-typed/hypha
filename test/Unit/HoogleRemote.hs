{-# LANGUAGE OverloadedStrings #-}
module Unit.HoogleRemote (tests) where

import qualified Data.ByteString.Lazy as LBS
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

import Hypha.Hoogle.Remote
  ( RemoteError (..), RemoteHoogleTransport (..), RemoteOptions (..)
  , defaultRemoteOptions, searchRemoteWith )
import Hypha.Hoogle.Type (HoogleHit (..), HoogleQuery (..))
import Hypha.Search.PackageCache (openPackageCacheAt)

stubBody :: LBS.ByteString
stubBody = "[{\"package\":{\"name\":\"foo\"},\"module\":{\"name\":\"Foo\"},\"item\":\"bar\",\"type\":\"a -> a\",\"docs\":\"\"}]"

tests :: TestTree
tests = testGroup "Unit.HoogleRemote"
  [ testCase "single GET, cached on second call" $
      withSystemTempDirectory "hypha-rh" $ \tmp -> do
        c <- openPackageCacheAt (tmp </> "g.db") Nothing
        counter <- newIORef (0 :: Int)
        let transport = RemoteHoogleTransport $ \_ -> do
              atomicModifyIORef' counter (\n -> (n + 1, ()))
              pure (Right stubBody)
            opts = defaultRemoteOptions
        r1 <- searchRemoteWith transport opts c (HoogleQuery "bar")
        r2 <- searchRemoteWith transport opts c (HoogleQuery "bar")
        seen <- readIORef counter
        seen @?= 1
        case (r1, r2) of
          (Right hs1, Right hs2) -> do
            hs1 @?= [HoogleHit "foo" "Foo" "bar" "a -> a" ""]
            hs2 @?= hs1
          _ -> assertFailure ("unexpected: " <> show (r1, r2))

  , testCase "offline mode short-circuits" $
      withSystemTempDirectory "hypha-rh" $ \tmp -> do
        c <- openPackageCacheAt (tmp </> "g.db") Nothing
        let transport = RemoteHoogleTransport $ \_ ->
              assertFailure "transport must not be called"
                >> pure (Left RemoteOffline)
            opts = defaultRemoteOptions { roOffline = True }
        r <- searchRemoteWith transport opts c (HoogleQuery "bar")
        case r of
          Left  RemoteOffline -> pure ()
          other               -> assertFailure ("unexpected: " <> show other)

  , testCase "malformed JSON returns RemoteDecode" $
      withSystemTempDirectory "hypha-rh" $ \tmp -> do
        c <- openPackageCacheAt (tmp </> "g.db") Nothing
        let transport = RemoteHoogleTransport $ \_ ->
              pure (Right "not json")
            opts = defaultRemoteOptions
        r <- searchRemoteWith transport opts c (HoogleQuery "bar")
        case r of
          Left (RemoteDecode _) -> pure ()
          other -> assertFailure ("unexpected: " <> show other)
  ]
