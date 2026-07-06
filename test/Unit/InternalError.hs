{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Regression coverage for the internal-error path.
--
-- Historical context: the @hypha@ binary used to wrap its /entire/
-- 'main' in @catchAny@, which also caught the 'System.ExitCode'
-- exceptions raised by @System.exitWith@ and by @optparse-applicative@
-- on @--help@ — producing a spurious second @INTERNAL_ERROR@ envelope
-- after every successful command.  The handler grew a @fromException@
-- re-throw dance to compensate.
--
-- The current design makes that dance unnecessary by construction:
-- 'Hypha.Cli.Run.runClientMain' / 'runServerMain' guard only the
-- 'runHypha' computation with 'Control.Exception.Safe.tryAny', and
-- nothing inside that region calls @System.exit*@ — rendering and
-- process exit happen outside it.  What remains to pin here:
--
-- 1. the shape of the @INTERNAL_ERROR@ envelope, and
-- 2. that both internal-error renderers terminate with the dedicated
--    'exitInternalError' code (exit 9), never return.
module Unit.InternalError (tests) where

import Control.Exception      (ErrorCall (..), toException, try)
import qualified Data.Aeson   as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified System.Exit  as System

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

import Hypha.Cli.Run     (processInternalError, reportInternalError)
import Hypha.Cli.Types
  ( ClientCommandTag (..), CommandTag (..), HyphaOptions (..) )
import Hypha.Exit        (exitInternalError, unExitCode)
import Hypha.Output.Json (encodeInternalErrorEnvelope)

-- | Options as parsed with no flags given.
plainOptions :: HyphaOptions
plainOptions = HyphaOptions
  { hoProjectDir       = Nothing
  , hoPackageOverrides = []
  , hoOffline          = False
  , hoHuman            = False
  , hoPrettyJson       = False
  , hoFull             = False
  , hoSelect           = Nothing
  , hoQuiet            = False
  , hoVerbose          = False
  }

tests :: TestTree
tests = testGroup "Unit.InternalError"
  [ testCase "INTERNAL_ERROR envelope carries command, code and exit 9" $ do
      let envelope = encodeInternalErrorEnvelope "lookup" "boom"
      case envelope of
        Aeson.Object obj -> do
          KM.lookup "schema"  obj @?= Just (Aeson.String "hypha/v0")
          KM.lookup "command" obj @?= Just (Aeson.String "lookup")
          KM.lookup "ok"      obj @?= Just (Aeson.Bool False)
          case KM.lookup "error" obj of
            Just (Aeson.Object err) -> do
              KM.lookup "code"    err @?= Just (Aeson.String "INTERNAL_ERROR")
              KM.lookup "message" err @?= Just (Aeson.String "boom")
              KM.lookup "exit_code" err
                @?= Just (Aeson.Number
                            (fromIntegral (unExitCode exitInternalError)))
            other -> assertFailure ("no error object: " <> show other)
        other -> assertFailure ("envelope is not an object: " <> show other)

  , testCase "processInternalError terminates with exitInternalError" $ do
      let crash = toException (ErrorCall "synthetic crash\n")
      r <- try (processInternalError plainOptions (ClientTag LookupCmd) crash)
      assertInternalExit r

  , testCase "reportInternalError terminates with exitInternalError" $ do
      let crash = toException (ErrorCall "synthetic crash\n")
      r <- try (reportInternalError crash)
      assertInternalExit r
  ]

-- | Both internal-error renderers must end in
-- @exitWith (toSystemExitCode exitInternalError)@ — i.e. from the
-- caller's perspective they raise @ExitFailure 9@ and never return.
assertInternalExit :: Either System.ExitCode () -> IO ()
assertInternalExit r = case r of
  Left (System.ExitFailure n)
    | n == unExitCode exitInternalError -> pure ()
    | otherwise ->
        assertFailure ("wrong exit code: " <> show n)
  Left System.ExitSuccess ->
    assertFailure "internal error exited with success"
  Right () ->
    assertFailure "internal-error renderer returned instead of exiting"
