{-# LANGUAGE LambdaCase #-}
module Hypha.Server.Slots
  ( BuildSlots
  , BuildState (..)
  , initialiseSlots
  , withSlot
  ) where

import Control.Concurrent.Async (Async, async, wait)
import Control.Concurrent.MVar  (MVar, newMVar, modifyMVar, modifyMVar_)
import Control.Exception        (SomeException, try)
import qualified Data.Map.Strict as Map
import Data.Map.Strict          (Map)

import Hypha.Types.PackageId (PackageId)

-- | Per-package build state for de-duplicated lazy Haddock builds.
data BuildState
  = NotStarted
  | Building !(Async FilePath)
  | Done     !FilePath
  | Failed   !SomeException

-- | Immutable map of per-package build slots. The outer 'Map' is built once
-- at server startup from the 'BuildPlan' and never modified afterwards.
-- Each per-package 'MVar' is an independent lock so concurrent requests for
-- different packages never contend.
type BuildSlots = Map PackageId (MVar BuildState)

-- | Build a fresh slot for every package in the given list.
initialiseSlots :: [PackageId] -> IO BuildSlots
initialiseSlots pids = do
  pairs <- mapM (\pid -> do mv <- newMVar NotStarted; pure (pid, mv)) pids
  pure (Map.fromList pairs)

-- | Acquire-or-spawn semantics. The first caller spawns the build action;
-- subsequent callers wait on the same 'Async'. If the 'PackageId' is not in
-- the slots map, the build is spawned with no caching.
--
-- The implementation uses a two-phase protocol:
--   1. Atomically check the 'MVar' — either return a cached result, or
--      grab the 'Async' to wait on and release the 'MVar' immediately.
--   2. Wait for the 'Async' outside the 'MVar'.
--   3. Cache the final state for future callers.
withSlot :: BuildSlots
         -> PackageId
         -> IO FilePath        -- ^ build action; returns the haddock dir on success
         -> IO (Either SomeException FilePath)
withSlot slots pid build = case Map.lookup pid slots of
  Nothing -> try build
  Just slot -> do
    -- Phase 1: check the slot atomically.  Return cached results immediately
    -- or grab the async to wait on and release the MVar right away.
    mr <- modifyMVar slot $ \case
      Done fp     -> pure (Done fp,     Right (Right fp))
      Failed e    -> pure (Failed e,    Right (Left  e))
      NotStarted  -> do a <- async build; pure (Building a, Left a)
      Building a  ->                            pure (Building a, Left a)
    case mr of
      Right r -> pure r
      Left a  -> awaitAsync slot a

-- | Wait for an asynchronous build, then cache the result in the slot so
-- that future callers see the cached 'Done' or 'Failed' state.
awaitAsync :: MVar BuildState -> Async FilePath -> IO (Either SomeException FilePath)
awaitAsync slot a = do
  r <- try (wait a)
  modifyMVar_ slot $ \_ -> pure $ case r of
    Right fp -> Done fp
    Left  e  -> Failed e
  pure r
