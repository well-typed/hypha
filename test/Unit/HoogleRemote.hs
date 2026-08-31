{-# LANGUAGE OverloadedStrings #-}
module Unit.HoogleRemote (tests) where

import qualified Data.ByteString.Lazy as LBS
import Data.IORef (atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as Text
import Network.HTTP.Client (parseUrlThrow)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

import Hypha.Hoogle.Remote
  ( RemoteError (..), RemoteHoogleTransport (..), RemoteOptions (..)
  , defaultRemoteOptions, searchRemoteWith )
import Hypha.Hoogle.Type (HoogleHit (..), HoogleQuery (..))
import Hypha.Search.PackageCache (openPackageCacheAt)

-- | Runs @q@ through the cascade with a transport that only records
-- the URL it was handed, and hands that URL back.
urlFor :: Text -> IO Text
urlFor q = withSystemTempDirectory "hypha-rh" $ \tmp -> do
  c    <- openPackageCacheAt (tmp </> "g.db") Nothing
  seen <- newIORef Nothing
  let transport = RemoteHoogleTransport $ \url -> do
        writeIORef seen (Just url)
        pure (Right stubBody)
  _   <- searchRemoteWith transport defaultRemoteOptions c (HoogleQuery q)
  got <- readIORef seen
  case got of
    Just url -> pure url
    Nothing  -> assertFailure "transport was never called" >> pure ""

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

  , testCase "a type-signature query encodes to a URL the client accepts" $ do
      url <- urlFor "Ord b => (a -> b) -> [a] -> [a]"
      url @?=
        "https://hoogle.haskell.org/?mode=json&count=20\
        \&hoogle=Ord%20b%20%3D%3E%20%28a%20-%3E%20b%29\
        \%20-%3E%20%5Ba%5D%20-%3E%20%5Ba%5D"
      -- http-client parses the URL before it opens a socket: an
      -- under-escaped query is an InvalidUrlException, not a 400.
      _ <- parseUrlThrow (Text.unpack url)
      pure ()

  , testCase "reserved characters survive as query data, not as syntax" $ do
      url <- urlFor "a&b=c?d#e"
      url @?=
        "https://hoogle.haskell.org/?mode=json&count=20\
        \&hoogle=a%26b%3Dc%3Fd%23e"
      _ <- parseUrlThrow (Text.unpack url)
      pure ()
  ]
