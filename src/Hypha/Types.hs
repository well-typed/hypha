{-# LANGUAGE DerivingStrategies #-}

module Hypha.Types (
    HyphaM(..)
  , Hypha
  , runHypha
  , askOpts
  , liftEitherIO
  , module Control.Monad.Reader
  , module Control.Monad.Except
  ) where

import Control.Monad.Reader
import Control.Monad.Except
import Hypha.Cli.Types
import Hypha.Error

-- | The hypha CLI monad: the parsed 'HyphaOptions' in a reader, typed
-- failure via 'HyphaError'.  The error carries no command tag — the tag
-- is a pure function of the parsed 'Command', so @Main@ attaches it at
-- the rendering boundary ('Hypha.Cli.Run.processError') instead of the
-- monad threading it through every computation.
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

-- | Run an 'IO' action that reports failure as 'Either', injecting the
-- error into the caller's 'MonadError' channel.  The injection function
-- names how the boundary error maps into the umbrella error type, e.g.
--
-- > root <- liftEitherIO DiscoveryFailure (discoverProjectRoot mDir)
--
-- Use @liftEitherIO id@ when the action already fails with the target
-- error type.
liftEitherIO
  :: (MonadIO m, MonadError err m)
  => (e -> err) -> IO (Either e a) -> m a
liftEitherIO inj action = liftIO action >>= either (throwError . inj) pure
