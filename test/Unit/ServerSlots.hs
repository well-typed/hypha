{-# LANGUAGE OverloadedStrings #-}
module Unit.ServerSlots (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Control.Concurrent.Async (concurrently)
import Data.IORef

import Hypha.Server.Slots (initialiseSlots, withSlot)
import Hypha.Types.PackageId (PackageId (..), PackageName (..), Version (..))

asyncPid :: PackageId
asyncPid = PackageId (PackageName "async") (Version "2.2.5")

tests :: TestTree
tests = testGroup "Server.Slots"
  [ testCase "build runs at most once for the same package" $ do
      counter <- newIORef (0 :: Int)
      slots <- initialiseSlots [asyncPid]
      let build = do
            atomicModifyIORef' counter (\n -> (n + 1, ()))
            pure "/tmp/haddock-async"
      (_, _) <- concurrently (withSlot slots asyncPid build)
                             (withSlot slots asyncPid build)
      n <- readIORef counter
      n @?= 1
  ]
