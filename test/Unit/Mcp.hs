{-# LANGUAGE OverloadedStrings #-}
module Unit.Mcp (tests) where

import qualified Data.Text as Text
import System.Exit (ExitCode (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Hypha.Mcp.Server (execHypha)

tests :: TestTree
tests = testGroup "Mcp"
  [ testCase "execHypha echo returns exit 0 + stdout" testExecEcho
  , testCase "execHypha sh exit 3 returns exit 3" testExecExitCode
  ]

testExecEcho :: IO ()
testExecEcho = do
  (ec, out, _err) <- execHypha "/bin/echo" ["hello"]
  case ec of
    ExitSuccess   -> pure ()
    ExitFailure _ -> fail ("expected exit 0, got: " ++ show ec)
  Text.takeWhile (/= '\n') out @?= "hello"

testExecExitCode :: IO ()
testExecExitCode = do
  (ec, _out, _err) <- execHypha "/bin/sh" ["-c", "exit 3"]
  case ec of
    ExitSuccess   -> fail ("expected exit 3, got: 0")
    ExitFailure n -> n @?= 3
