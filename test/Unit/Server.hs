{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Unit.Server (tests) where

import Control.Exception (SomeException, evaluate, try)
import Data.IP (IP (..), toIPv4, toIPv6)
import Data.Text qualified as Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)
import Text.RawString.QQ (r)

import Hypha.Command.Server
  ( BindAddr (..), BindError (..), briefException, collectModuleRows, parseBind )

mkIPv4 :: [Int] -> IP
mkIPv4 = IPv4 . toIPv4

mkIPv6 :: [Int] -> IP
mkIPv6 = IPv6 . toIPv6

tests :: TestTree
tests = testGroup "Unit.Server"
  [ testGroup "Server.parseBind" parseBindTests
  , testGroup "Server.collectModuleRows" collectModuleRowsTests
  , testGroup "Server.briefException" briefExceptionTests
  ]

parseBindTests :: [TestTree]
parseBindTests =
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

collectModuleRowsTests :: [TestTree]
collectModuleRowsTests =
  [ testCase "ordinary module yields a row per top-level decl" $ do
      rows <- collectModuleRows "pkg-1.0:lib" "Foo" "Foo.hs" plainSrc
      assertBool "expected a row for foo" (any (\(_, _, nm, _) -> nm == "foo") rows)

  , testCase "CPP #error on a build-time-only macro is skipped, not thrown" $ do
      -- Mirrors OneTuple's Data.Tuple.Solo.TH: a hard #error guarding a
      -- macro (CURRENT_PACKAGE_KEY) only a real GHC invocation defines.
      -- hypha's cpphs pass has no compiler session, so this can never
      -- succeed — the regression is the exception escaping and
      -- aborting every other package's indexing, not this one module.
      rows <- collectModuleRows "pkg-1.0:lib" "Foo" "Foo.hs" cppErrorSrc
      rows @?= []
  ]
  where
    plainSrc = Text.pack [r|module Foo where

foo :: Int
foo = 1
|]
    cppErrorSrc = Text.pack [r|{-# LANGUAGE CPP #-}
module Foo where

#ifndef CURRENT_PACKAGE_KEY
#error "CURRENT_PACKAGE_KEY undefined"
#endif

foo :: Int
foo = 1
|]

briefExceptionTests :: [TestTree]
briefExceptionTests =
  [ testCase "ErrorCall message preserved exactly, CallStack stripped" $ do
      -- GHC (>= 9.10) attaches a CallStack to every 'error' call in two
      -- places: as a legacy location string inside ErrorCall, and as a
      -- Backtraces annotation in SomeException's ExceptionContext.
      -- The ErrorCall pattern synonym discards the location; the
      -- SomeException pattern match drops the context.  What remains is
      -- exactly the message string passed to 'error'.
      Left (e :: SomeException) <- try (evaluate (error "boom" :: ()))
      briefException e @?= "boom"

  , testCase "error message containing the CallStack header is not truncated" $ do
      -- Regression: the old string-matching approach searched for
      -- "CallStack (from HasCallStack)" in the rendered output and
      -- truncated there.  An error message that /itself/ starts with
      -- that string would be truncated to empty.  The structured
      -- approach extracts the message via the ErrorCall pattern synonym,
      -- which is immune to this class of bug.
      Left (e :: SomeException) <-
        try (evaluate (error [r|CallStack (from HasCallStack): oops|] :: ()))
      briefException e @?= "CallStack (from HasCallStack): oops"

  , testCase "multi-line ErrorCall message is collapsed to one line" $ do
      Left (e :: SomeException) <-
        try (evaluate (error [r|line1
line2
line3|] :: ()))
      briefException e @?= "line1 line2 line3"

  , testCase "non-ErrorCall exception message is preserved" $ do
      result <- try (evaluate (1 `div` 0 :: Int)) :: IO (Either SomeException Int)
      case result of
        Left e  -> briefException e @?= "divide by zero"
        Right _ -> assertFailure "division by zero must throw"
  ]