{-# LANGUAGE OverloadedStrings #-}
-- | Tests for 'HyphaError' classification and exit-code mapping.
--
-- Pinned to the regression that motivated the @TOOL_MISSING@ variant: a
-- live @hypha lookup@ run inside Claude Code's sandbox raised an
-- @ENOENT@ for the @haddock@ binary (because the toolchain directory
-- was hidden from the sandboxed view), and the CLI misreported it as
-- @NETWORK_ERROR@.  The classifier here must turn @ENOENT@ from a
-- child-process spawn into 'ToolMissing' so the cascade and exit code
-- carry the right signal.
module Unit.Errors (tests) where

import Control.Exception (ErrorCall (..), SomeException, toException)
import GHC.IO.Exception
  ( IOErrorType (NoSuchThing), IOException (..) )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Hypha.Cli.Run (classifyLookupException)
import Hypha.Error (HyphaError (..), errorCode, errorExitCode)
import Hypha.Exit (unExitCode)

tests :: TestTree
tests = testGroup "Unit.Errors"
  [ testCase "ENOENT from a child-process spawn maps to ToolMissing" $
      case classifyLookupException enoentSpawn of
        ToolMissing _ -> pure ()
        other         -> fail ("expected ToolMissing, got: " <> show other)

  , testCase "non-IO exception falls back to NetworkError" $
      case classifyLookupException (toException (ErrorCall "boom")) of
        NetworkError _ -> pure ()
        other          -> fail ("expected NetworkError, got: " <> show other)

  , testCase "TOOL_MISSING uses exit code 8" $
      unExitCode (errorExitCode (ToolMissing "haddock")) @?= 8

  , testCase "TOOL_MISSING is its own wire code" $
      errorCode (ToolMissing "haddock") @?= "TOOL_MISSING"
  ]

-- | A 'SomeException' shaped like the @posix_spawnp@ ENOENT that
-- 'System.Process.readProcessWithExitCode' raises when the requested
-- binary is missing from @PATH@.
enoentSpawn :: SomeException
enoentSpawn = toException IOError
  { ioe_handle      = Nothing
  , ioe_type        = NoSuchThing
  , ioe_location    = "readCreateProcessWithExitCode: posix_spawnp"
  , ioe_description = "does not exist"
  , ioe_errno       = Nothing
  , ioe_filename    = Just "haddock"
  }

