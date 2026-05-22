{-# LANGUAGE OverloadedStrings #-}
module Unit.Mcp (tests) where

import Data.Aeson (object, (.=))
import Data.Text (Text)
import qualified Data.Text as Text
import System.Exit (ExitCode (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertFailure, (@?=))

import Hypha.Mcp.Server (execHypha)
import Hypha.Mcp.Tools (argvForTool)

tests :: TestTree
tests = testGroup "Mcp"
  [ testCase "execHypha echo returns exit 0 + stdout" testExecEcho
  , testCase "execHypha sh exit 3 returns exit 3" testExecExitCode
  , testGroup "argvForTool"
      [ testCase "lookup → ['lookup', query]" testLookupArgv
      , testCase "symbol with select projects globals first" testSymbolSelect
      , testCase "deps with reverse + depth"  testDepsReverseDepth
      , testCase "doctor with no fields"      testDoctorEmpty
      , testCase "exec passes argv through"   testExecPassthrough
      , testCase "unknown tool yields Left"   testUnknownTool
      , testCase "missing required field"     testMissingRequired
      , testCase "wrong type for field"       testWrongType
      , testCase "global flags ordering"      testGlobalFlagOrdering
      ]
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

expectRight :: Either Text a -> IO a
expectRight = either (assertFailure . Text.unpack) pure

expectLeft :: Show a => Either Text a -> IO ()
expectLeft = either (const (pure ())) (assertFailure . ("expected Left, got: " ++) . show)

testLookupArgv :: IO ()
testLookupArgv = do
  argv <- expectRight (argvForTool "hypha.lookup"
            (object [ "query" .= ("filterM" :: Text) ]))
  argv @?= ["lookup", "filterM"]

testSymbolSelect :: IO ()
testSymbolSelect = do
  argv <- expectRight (argvForTool "hypha.symbol"
            (object
              [ "path"   .= ("aeson/Data.Aeson/encode" :: Text)
              , "select" .= ("sig,haddock" :: Text)
              ]))
  argv @?= ["--select", "sig,haddock", "symbol", "aeson/Data.Aeson/encode"]

testDepsReverseDepth :: IO ()
testDepsReverseDepth = do
  argv <- expectRight (argvForTool "hypha.deps"
            (object
              [ "pkg"     .= ("mtl" :: Text)
              , "reverse" .= True
              , "depth"   .= (2 :: Int)
              ]))
  argv @?= ["deps", "mtl", "--reverse", "--depth", "2"]

testDoctorEmpty :: IO ()
testDoctorEmpty = do
  argv <- expectRight (argvForTool "hypha.doctor" (object []))
  argv @?= ["doctor"]

testExecPassthrough :: IO ()
testExecPassthrough = do
  argv <- expectRight (argvForTool "hypha.exec"
            (object [ "args" .= ([ "lookup", "filterM" ] :: [Text]) ]))
  argv @?= ["lookup", "filterM"]

testUnknownTool :: IO ()
testUnknownTool =
  expectLeft (argvForTool "hypha.bogus" (object []))

testMissingRequired :: IO ()
testMissingRequired =
  expectLeft (argvForTool "hypha.lookup" (object []))

testWrongType :: IO ()
testWrongType =
  expectLeft (argvForTool "hypha.lookup"
                (object [ "query" .= (42 :: Int) ]))

testGlobalFlagOrdering :: IO ()
testGlobalFlagOrdering = do
  argv <- expectRight (argvForTool "hypha.package"
            (object
              [ "pkg"        .= ("text" :: Text)
              , "offline"    .= True
              , "projectDir" .= ("/tmp/proj" :: Text)
              ]))
  argv @?= ["--project-dir", "/tmp/proj", "--offline", "package", "text"]
