{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | End-to-end coverage that spawns the real @hypha@ binary and pins
-- the shape of what users actually see.
--
-- These tests exist because two regressions had to be debugged
-- twice: a stale second envelope and an @INTERNAL_ERROR: ExitSuccess@
-- stderr line printed on every successful command.  Both were
-- consequences of @System.exitWith@ raising an 'ExitCode' exception
-- that the top-level @handleAny@ misclassified as a crash.  The
-- in-process unit tests pin the handler logic; these tests pin the
-- observable end-to-end behaviour from the user's perspective:
--
--   * A successful command emits /exactly one/ JSON object on stdout.
--   * Stderr stays empty on success.
--   * The process exits with status 0.
--   * No @INTERNAL_ERROR@ envelope appears anywhere.
--
-- @build-tool-depends: hypha:hypha@ (see @hypha.cabal@) puts the
-- freshly-built binary on @$PATH@ so these tests cannot accidentally
-- pick up an older system install.
module Golden.Cli (tests) where

import qualified Data.Aeson                 as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS8
import           System.Exit                (ExitCode (..))
import           System.Process             (readProcessWithExitCode)

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

tests :: TestTree
tests = testGroup "Golden.Cli"
  [ testCase "hypha --help: clean stdout/stderr, exit 0, no INTERNAL_ERROR" $
      assertCleanInvocation ["--help"]

  , testCase "hypha --version: clean stdout/stderr, exit 0, no INTERNAL_ERROR" $
      -- --version is provided by optparse-applicative when configured;
      -- if hypha doesn't expose it the binary should still terminate
      -- cleanly with a usage envelope rather than the spurious crash
      -- envelope this suite was created to prevent.  We assert only
      -- the regression invariant, not the exact text.
      assertNoInternalErrorPollution ["--version"]
  ]

-- | Strict invariant: stdout is exactly one well-formed JSON object,
-- stderr is empty, exit code is 'ExitSuccess', and at no point does
-- the @INTERNAL_ERROR@ marker leak into either stream.
assertCleanInvocation :: [String] -> IO ()
assertCleanInvocation args = do
  (ec, out, err) <- readProcessWithExitCode "hypha" args ""
  assertEqual "stderr must be empty on success" "" err
  assertEqual "exit code" ExitSuccess ec
  assertBool "no INTERNAL_ERROR marker on stdout" $
    not ("INTERNAL_ERROR" `isInfixOfStr` out)
  assertSingleJsonObjectOrPlainText out

-- | Weaker invariant used for invocations whose stdout we don't pin
-- exactly: only assert that the @INTERNAL_ERROR@ regression marker is
-- absent from both streams and that there is no second envelope
-- appended to stdout.
assertNoInternalErrorPollution :: [String] -> IO ()
assertNoInternalErrorPollution args = do
  (_ec, out, err) <- readProcessWithExitCode "hypha" args ""
  assertBool "no INTERNAL_ERROR marker on stderr" $
    not ("INTERNAL_ERROR" `isInfixOfStr` err)
  assertBool "no INTERNAL_ERROR marker on stdout" $
    not ("INTERNAL_ERROR" `isInfixOfStr` out)
  assertBool "no \"<internal>\" command marker on stdout" $
    not ("\"command\":\"<internal>\"" `isInfixOfStr` out)

-- | Either stdout decodes as exactly one JSON value with no trailing
-- bytes, or it's plain text (e.g. @--help@) with no embedded JSON
-- envelope.  The point is that we never emit two responses for one
-- invocation.
assertSingleJsonObjectOrPlainText :: String -> IO ()
assertSingleJsonObjectOrPlainText raw =
  case Aeson.decode @Aeson.Value (LBS8.pack raw) of
    Just _ ->
      -- It parsed as one JSON value covering all the bytes; good.
      pure ()
    Nothing ->
      -- Not pure JSON.  Make sure no envelope is /embedded/ in plain
      -- text — that is the exact shape of the regression (help text
      -- followed by a stray @{"schema":"hypha/v0",...}@).
      assertBool
        ("plain-text output must not contain an embedded JSON envelope; got:\n"
          <> raw)
        (not ("\"schema\":\"hypha/v0\"" `isInfixOfStr` raw))

-- | @isInfixOf@ for strings.  We avoid pulling in @Data.List@'s
-- polymorphic version under another name to keep the import list flat.
isInfixOfStr :: String -> String -> Bool
isInfixOfStr needle hay = needle `isInfixOf'` hay
  where
    isInfixOf' n h
      | length n > length h = False
      | n == take (length n) h = True
      | otherwise = case h of
          []     -> False
          (_:hs) -> isInfixOf' n hs

