{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Regression coverage for the top-level @catchAny@ in the @hypha@
-- binary.  @System.exitWith@ — and @optparse-applicative@ on
-- @--help@/@--version@ — signal a clean exit by /raising/ an
-- 'ExitCode' exception.  Because 'Control.Exception.Safe.handleAny'
-- catches every synchronous exception, that signal previously landed
-- in 'reportInternalError', producing a spurious second envelope on
-- stdout and an @INTERNAL_ERROR: ExitSuccess@ line on stderr at the
-- end of every successful command.
--
-- 'topLevelHandler' filters 'ExitCode' out of the handler so it
-- propagates to the runtime untouched; only genuine crashes are
-- rendered via 'reportInternalError'.  These tests pin that contract
-- so the regression cannot re-emerge unnoticed.
module Unit.InternalError (tests) where

import Control.Exception      (ErrorCall (..), toException, try)
import qualified System.Exit  as System

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)

import Hypha.Cli.Run (topLevelHandler)

tests :: TestTree
tests = testGroup "Unit.InternalError"
  [ testCase "topLevelHandler re-throws ExitSuccess verbatim" $ do
      r <- try (topLevelHandler (toException System.ExitSuccess))
      case r of
        Left System.ExitSuccess -> pure ()
        Left other ->
          assertFailure ("expected ExitSuccess, got " <> show other)
        Right () ->
          assertFailure
            "ExitSuccess was swallowed instead of being re-thrown — \
            \this is the regression that caused the spurious \
            \INTERNAL_ERROR envelope on every successful command."

  , testCase "topLevelHandler re-throws ExitFailure verbatim" $ do
      let ec = System.ExitFailure 7
      r <- try (topLevelHandler (toException ec))
      case r of
        Left e | e == ec -> pure ()
        Left other ->
          assertFailure ("expected " <> show ec <> ", got " <> show other)
        Right () ->
          assertFailure "ExitFailure was swallowed instead of being re-thrown"

  , testCase "non-ExitCode crash routed to INTERNAL_ERROR path" $ do
      -- reportInternalError prints its envelope and then calls
      -- System.exitWith exitInternalError, so from the test's
      -- perspective topLevelHandler raises an 'ExitCode' (an
      -- ExitFailure carrying 'exitInternalError') rather than
      -- returning.  We pin only the rethrown ExitCode shape here;
      -- the verbatim-rethrow tests above distinguish the
      -- "intended exit" path from this "we crashed" path because the
      -- input exception there is 'System.ExitSuccess' while here it
      -- is an 'ErrorCall'.
      r <- try (topLevelHandler (toException (ErrorCall "synthetic crash")))
      case r of
        Left (_ :: System.ExitCode) -> pure ()
        Right () ->
          assertFailure
            "non-ExitCode crash returned normally — \
            \topLevelHandler must always terminate with an ExitCode."
  ]
