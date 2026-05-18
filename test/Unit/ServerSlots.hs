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

expectedBuildResult :: FilePath
expectedBuildResult = "/tmp/haddock-async"

-- | A build action that increments a counter and returns a fixed path.
mkBuild :: IORef Int -> IO FilePath
mkBuild counter = do
  atomicModifyIORef' counter (\n -> (n + 1, ()))
  pure expectedBuildResult

-- | A build action that increments a counter then fails with an exception.
mkFailingBuild :: IORef Int -> IO FilePath
mkFailingBuild counter = do
  atomicModifyIORef' counter (\n -> (n + 1, ()))
  fail "i/o error"

tests :: TestTree
tests = testGroup "Server.Slots"
  [ testCase "two concurrent calls invoke build at most once" $ do
      counter <- newIORef (0 :: Int)
      slots <- initialiseSlots [asyncPid]
      (_, _) <- concurrently (withSlot slots asyncPid (mkBuild counter))
                             (withSlot slots asyncPid (mkBuild counter))
      n <- readIORef counter
      n @?= 1

  , testCase "serial calls return cached result" $ do
      counter <- newIORef (0 :: Int)
      slots <- initialiseSlots [asyncPid]
      r1 <- withSlot slots asyncPid (mkBuild counter)
      r2 <- withSlot slots asyncPid (mkBuild counter)
      n <- readIORef counter
      n @?= 1
      case (r1, r2) of
        (Right fp1, Right fp2) -> do
          fp1 @?= expectedBuildResult
          fp2 @?= expectedBuildResult
        _ -> assertFailure $ "expected Right, got " <> show r1 <> " and " <> show r2

  , testCase "build failure is propagated and cached" $ do
      counter <- newIORef (0 :: Int)
      slots <- initialiseSlots [asyncPid]
      r1 <- withSlot slots asyncPid (mkFailingBuild counter)
      r2 <- withSlot slots asyncPid (mkFailingBuild counter)
      n <- readIORef counter
      -- Build ran exactly once; both calls got the failure
      n @?= 1
      case (r1, r2) of
        (Left _, Left _) -> pure ()
        _                -> assertFailure "expected Left for both calls"

  , testCase "three concurrent calls invoke build once" $ do
      counter <- newIORef (0 :: Int)
      slots <- initialiseSlots [asyncPid]
      (_, (_, _)) <- concurrently
                       (withSlot slots asyncPid (mkBuild counter))
                       (concurrently
                         (withSlot slots asyncPid (mkBuild counter))
                         (withSlot slots asyncPid (mkBuild counter)))
      n <- readIORef counter
      n @?= 1

  , testCase "missing package runs build with no caching" $ do
      counter <- newIORef (0 :: Int)
      slots <- initialiseSlots []  -- no packages registered
      (_, _) <- concurrently (withSlot slots asyncPid (mkBuild counter))
                             (withSlot slots asyncPid (mkBuild counter))
      n <- readIORef counter
      -- Both calls ran the build independently (no slot to cache)
      n @?= 2
  ]
