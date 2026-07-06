{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Logging
  ( -- * Types
    LogEvent (..)
  , Tracer
    -- * Tracer construction
  , silentTracer
  , verboseTracer
  ) where

import Control.Monad.IO.Class
import Data.Text.IO qualified as TIO
import Data.Text (Text)
import System.IO (hFlush, stderr)

type Tracer m = LogEvent -> m ()

-- | Events that can be logged.
data LogEvent
  = LogInfo !Text
    -- ^ Informational message.
  | LogDebug !Text
    -- ^ Debug message (only shown with @--verbose@).
  | LogWarning !Text
    -- ^ Warning message.
  deriving stock (Show, Eq)

-- | A tracer that discards all events.
silentTracer :: Monad m => Tracer m
silentTracer _ = pure ()

-- | A tracer that prints all events to stderr.
verboseTracer :: MonadIO m => Tracer m
verboseTracer event = liftIO $ do
  TIO.hPutStrLn stderr (formatEvent event)
  hFlush stderr

-- | Format a log event for display.
formatEvent :: LogEvent -> Text
formatEvent (LogInfo msg)    = "[info] " <> msg
formatEvent (LogDebug msg)   = "[debug] " <> msg
formatEvent (LogWarning msg) = "[warning] " <> msg
