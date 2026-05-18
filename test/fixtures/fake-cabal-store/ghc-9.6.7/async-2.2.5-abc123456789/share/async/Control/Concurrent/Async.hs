module Control.Concurrent.Async
  ( Async
  , async
  , wait
  , cancel
  , concurrently
  , race
  ) where

-- | A handle to an asynchronous action.
data Async a = Async

-- | Spawn an asynchronous action.
async :: IO a -> IO (Async a)
async = undefined

-- | Wait for an asynchronous action to complete.
wait :: Async a -> IO a
wait = undefined

-- | Cancel an asynchronous action.
cancel :: Async a -> IO ()
cancel = undefined

-- | Run two @IO@ actions concurrently.  Both actions start immediately in
-- separate threads.  When both complete, their results are returned as a pair.
--
-- This is the bread-and-butter concurrency primitive for the @async@ package.
concurrently :: IO a -> IO b -> IO (a, b)
concurrently = undefined

-- | Run two @IO@ actions concurrently and return the result of the one
-- that finishes first.
race :: IO a -> IO b -> IO (Either a b)
race = undefined
