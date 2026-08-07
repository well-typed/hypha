{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
-- | Tests for the argument parser itself.
--
-- 'Hypha.Cli.Parser.parseCli' runs 'execParser', which terminates the
-- process, so the parser is exercised through the 'ParserInfo' it is
-- built from and @optparse-applicative@'s pure driver.
module Unit.CliParser (tests) where

import Data.List (isInfixOf)
import Options.Applicative
  ( ParserResult (..), defaultPrefs, execParserPure, renderFailure )
import System.Exit (ExitCode (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

import Hypha.Cli.Parser (cliParserInfo)
import Hypha.Cli.Types (HyphaOptions (..), TimeoutSeconds (..), timeoutMicros)

tests :: TestTree
tests = testGroup "Unit.CliParser"
  [ testCase "--version reports the version and exits successfully" $
      -- The first thing anyone types after installing.  It has to exit
      -- 0: a version probe that exits non-zero reads as "this build is
      -- broken" to a shell script and to a person alike.
      case execParserPure defaultPrefs cliParserInfo ["--version"] of
        Failure f -> do
          let (msg, code) = renderFailure f "hypha"
          code @?= ExitSuccess
          assertBool ("expected a version in: " <> msg)
                     ("hypha " `isInfixOf` msg)
        _ -> assertFailure "--version should short-circuit the parse"

  , testCase "-V is the same as --version" $
      case execParserPure defaultPrefs cliParserInfo ["-V"] of
        Failure f -> snd (renderFailure f "hypha") @?= ExitSuccess
        _ -> assertFailure "-V should short-circuit the parse"

  , testGroup "--hoogle-timeout"
      -- Offered by hypha as the remediation on a remote-tier failure, so
      -- it has to move the timeout it names.
      [ testCase "a positive number of seconds parses" $
          hoogleTimeoutOf ["lookup", "fmap", "--hoogle-timeout", "30"]
            @?= Just (TimeoutSeconds 30)

      , testCase "absent means absent, not zero" $
          hoogleTimeoutOf ["lookup", "fmap"] @?= Nothing

      , testCase "seconds convert to the microseconds the client wants" $
          timeoutMicros (TimeoutSeconds 30) @?= 30_000_000

      , testCase "a non-positive or non-numeric value is rejected" $
          -- Rejected at the parser rather than defaulted: a silent
          -- fallback means the retry hypha suggested fails identically
          -- and says nothing about why.
          mapM_ (\v -> assertBool (show v <> " should be rejected")
                        (parseFails ["lookup", "fmap", "--hoogle-timeout", v]))
                ["0", "-5", "abc", "3.5", ""]
      ]
  ]

-- | The @--hoogle-timeout@ of a successful parse.
hoogleTimeoutOf :: [String] -> Maybe TimeoutSeconds
hoogleTimeoutOf argv =
  case execParserPure defaultPrefs cliParserInfo argv of
    Success (opts, _) -> hoHoogleTimeout opts
    _                 -> error ("expected a successful parse of " <> show argv)

parseFails :: [String] -> Bool
parseFails argv = case execParserPure defaultPrefs cliParserInfo argv of
  Success _ -> False
  _         -> True

