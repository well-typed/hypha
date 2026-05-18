{-# LANGUAGE OverloadedStrings #-}
module Unit.ServerSlots (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertFailure)
import Control.Concurrent.Async (concurrently)
import Data.IORef

import Hypha.Server.Slots (initialiseSlots, withSlot)
import Hypha.Types.PackageId (PackageId (..), PackageName (..), Version (..))

asyncPid :: PackageId
asyncPid = PackageId (PackageName "async") (Version "2.2.5")

tests :: TestTree
tests = testGroup "Server.Slots"
  [ testCase "two concurrent calls invoke build at most once" $ do
      counter <- newIORef (0 :: Int)
      slots <- initialiseSlots [asyncPid]
      let build = do
            atomicModifyIORef' counter (\n -> (n + 1, ()))
            pure "/tmp/haddock-async"
      (_, _) <- concurrently (withSlot slots asyncPid build)
                             (withSlot slots asyncPid build)
      n <- readIORef counter
      n @?= 1

  , testCase "serial calls return cached result" $ do
      counter <- newIORef (0 :: Int)
      slots <- initialiseSlots [asyncPid]
      let build = do
            atomicModifyIORef' counter (\n -> (n + 1, ()))
            pure "/tmp/haddock-async"
      r1 <- withSlot slots asyncPid build
      r2 <- withSlot slots asyncPid build
      n <- readIORef counter
      n @?= 1
      case (r1, r2) of
        (Right fp1, Right fp2) -> do
          fp1 @?= "/tmp/haddock-async"
          fp2 @?= "/tmp/haddock-async"
        _ -> assertFailure "expected Right"

  , testCase "build failure is propagated and cached" $ do
      counter <- newIORef (0 :: Int)
      slots <- initialiseSlots [asyncPid]
      let build = do
            atomicModifyIORef' counter (\n -> (n + 1, ()))
            fail "i/o error"
      r1 <- withSlot slots asyncPid build
      r2 <- withSlot slots asyncPid build
      n <- readIORef counter
      -- Build ran exactly once; both calls got the failure
      n @?= 1
      case (r1, r2) of
        (Left _, Left _) -> pure ()
        _                -> assertFailure "expected Left"

  , testCase "three concurrent calls invoke build once" $ do
      counter <- newIORef (0 :: Int)
      slots <- initialiseSlots [asyncPid]
      let build = do
            atomicModifyIORef' counter (\n -> (n + 1, ()))
            pure "/tmp/haddock-async"
      (_, (_, _)) <- concurrently
                       (withSlot slots asyncPid build)
                       (concurrently
                         (withSlot slots asyncPid build)
                         (withSlot slots asyncPid build))
      n <- readIORef counter
      n @?= 1

  , testCase "missing package runs build with no caching" $ do
      counter <- newIORef (0 :: Int)
      slots <- initialiseSlots []  -- no packages registered
      let build = do
            atomicModifyIORef' counter (\n -> (n + 1, ()))
            pure "/tmp/haddock-async"
      (_, _) <- concurrently (withSlot slots asyncPid build)
                             (withSlot slots asyncPid build)
      n <- readIORef counter
      -- Both calls ran the build independently (no slot to cache)
      n @?= 2
  ]
