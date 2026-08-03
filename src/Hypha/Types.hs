{-# LANGUAGE DerivingStrategies #-}

module Hypha.Types (
    HyphaM(..)
  , Hypha
  , runHypha
  , HyphaEnv(..)
  , askOpts
  , trace
  , mapEitherIO
  , liftEitherIO
  , module Control.Monad.Reader
  , module Control.Monad.Except
  ) where

import Control.Monad.Reader
import Control.Monad.Except
import Hypha.Cli.Types
import Hypha.Error
import Hypha.Logging (Tracer, LogEvent, silentTracer, verboseTracer)
import Hypha.Cache (cacheRoot)

-- | Environment carried by 'HyphaM'.
data HyphaEnv m = HyphaEnv
  { heOptions :: !HyphaOptions
  , heTracer  :: Tracer m
  , heCacheDir :: !FilePath
  }

-- | The hypha CLI monad: 'HyphaEnv' in a reader, typed failure via
-- 'HyphaError'.
newtype HyphaM m a = HyphaM { _HyphaM :: ReaderT (HyphaEnv m) (ExceptT HyphaError m) a }
  deriving newtype
    ( Functor, Applicative, Monad
    , MonadReader (HyphaEnv m), MonadError HyphaError, MonadIO
    )

instance MonadTrans HyphaM where
  lift = HyphaM . lift . lift

type Hypha = HyphaM IO

-- | Construct the environment and run a 'HyphaM' computation to
-- completion.  The tracer is chosen once, here — no per-call allocation.
--
-- @--quiet@ wins over @--verbose@.  It had no effect at all before: the
-- flag was parsed, stored, and read nowhere, while the docs listed it as
-- suppressing informational output.
runHypha :: MonadIO m => HyphaOptions -> HyphaM m a -> m (Either HyphaError a)
runHypha opts (HyphaM m) = do
  resolvedCacheDir <- liftIO $ maybe cacheRoot pure (hoCacheDir opts)
  let verbose = hoVerbose opts && not (hoQuiet opts)
      env = HyphaEnv opts (if verbose then verboseTracer else silentTracer)
                     resolvedCacheDir
  runExceptT (runReaderT m env)

-- | Get the 'HyphaOptions' from the environment.
askOpts :: MonadReader (HyphaEnv m) m' => m' HyphaOptions
askOpts = asks heOptions

-- | Trace a 'LogEvent' using the 'Tracer' stored in the environment.
-- The tracer runs in the base monad @m@; 'lift' (via 'MonadTrans')
-- hoists it into 'HyphaM'.
trace :: Monad m => LogEvent -> HyphaM m ()
trace event = do
  env <- ask
  lift (heTracer env event)

-- | Distant cousin in spirit of 'withExcept' and 'mapExcept'.
-- Run an 'IO' action that reports failure as 'Either', injecting the
-- error into the caller's 'MonadError' channel.  The injection function
-- names how the boundary error maps into the umbrella error type, e.g.
--
-- > root <- mapEitherIO DiscoveryFailure (discoverProjectRoot mDir)
--
-- Use @mapEitherIO id@ when the action already fails with the target
-- error type.
mapEitherIO :: (MonadIO m, MonadError err m) => (e -> err) -> IO (Either e a) -> m a
mapEitherIO inj action = liftIO action >>= either (throwError . inj) pure

liftEitherIO :: (MonadIO m, MonadError e m) => IO (Either e a) -> m a
liftEitherIO = mapEitherIO id
