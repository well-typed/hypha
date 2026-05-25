{-# LANGUAGE OverloadedStrings #-}
module Unit.Server (tests) where

import Data.IP (IP (..), toIPv4, toIPv6)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

import Hypha.Command.Server
  ( BindAddr (..), BindError (..), parseBind )

mkIPv4 :: [Int] -> IP
mkIPv4 = IPv4 . toIPv4

mkIPv6 :: [Int] -> IP
mkIPv6 = IPv6 . toIPv6

tests :: TestTree
tests = testGroup "Server.parseBind"
  [ testCase "localhost:4287 accepted" $
      parseBind "localhost:4287" @?= Right (BindAddr (mkIPv4 [127,0,0,1]) 4287)

  , testCase "127.0.0.1:4287 accepted" $
      parseBind "127.0.0.1:4287" @?= Right (BindAddr (mkIPv4 [127,0,0,1]) 4287)

  , testCase "127.0.0.5:4287 accepted (within 127.0.0.0/8)" $
      parseBind "127.0.0.5:4287" @?= Right (BindAddr (mkIPv4 [127,0,0,5]) 4287)

  , testCase "[::1]:4287 accepted" $
      parseBind "[::1]:4287" @?= Right (BindAddr (mkIPv6 [0,0,0,0,0,0,0,1]) 4287)

  , testCase "0.0.0.0:4287 refused" $
      parseBind "0.0.0.0:4287" @?= Left (BindNonLoopback "0.0.0.0:4287")

  , testCase "public IP refused" $
      parseBind "192.168.1.10:4287" @?= Left (BindNonLoopback "192.168.1.10:4287")

  , testCase "[2001:db8::1]:4287 refused (non-loopback IPv6)" $
      parseBind "[2001:db8::1]:4287" @?= Left (BindNonLoopback "[2001:db8::1]:4287")

  , testCase "malformed input refused" $
      parseBind "not-a-bind" @?= Left (BindMalformed "not-a-bind")

  , testCase "non-numeric port refused" $
      parseBind "127.0.0.1:zzz" @?= Left (BindMalformed "127.0.0.1:zzz")

  , testCase "out-of-range port refused" $
      parseBind "127.0.0.1:70000" @?= Left (BindMalformed "127.0.0.1:70000")

  , testCase "bare IPv6 without brackets rejected (ambiguous)" $
      parseBind "::1:4287" @?= Left (BindMalformed "::1:4287")
  ]
