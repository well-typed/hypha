{-# LANGUAGE DerivingStrategies #-}

module Hypha.Types (
    HyphaM(..)
  , Hypha
  , runHypha
  , askOpts
  , mapEitherIO
  , liftEitherIO
  , module Control.Monad.Reader
  , module Control.Monad.Except
  ) where

import Control.Monad.Reader
import Control.Monad.Except
import Hypha.Cli.Types
import Hypha.Error

-- | The hypha CLI monad: the parsed 'HyphaOptions' in a reader, typed
-- failure via 'HyphaError'.
newtype HyphaM m a = HyphaM { _HyphaM :: ReaderT HyphaOptions (ExceptT HyphaError m) a }
  deriving newtype
    ( Functor, Applicative, Monad
    , MonadReader HyphaOptions, MonadError HyphaError, MonadIO
    )

type Hypha = HyphaM IO

runHypha :: HyphaOptions -> HyphaM m a -> m (Either HyphaError a)
runHypha opts (HyphaM m) = runExceptT (runReaderT m opts)

-- | Get the 'HyphaOptions' from the environment.
askOpts :: MonadReader HyphaOptions m => m HyphaOptions
askOpts = ask

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
