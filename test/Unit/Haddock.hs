{-# LANGUAGE OverloadedStrings #-}
module Unit.Haddock (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import System.FilePath (takeFileName)

import Hypha.Haddock.Generate (haddockDirFor)
import Hypha.Types.PackageId (PackageId (..), PackageName (..), Version (..))

tests :: TestTree
tests = testGroup "Haddock"
  [ testCase "haddockDirFor uses pkg-ver naming" testDirNaming
  ]

testDirNaming :: IO ()
testDirNaming = do
  let pid = PackageId (PackageName "async") (Version "2.2.5")
  dir <- haddockDirFor pid
  takeFileName dir @?= "async-2.2.5"
