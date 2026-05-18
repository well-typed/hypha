{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Logging
  ( -- * Types
    LogEvent (..)
    -- * Tracer construction
  , silentTracer
  , verboseTracer
  ) where

import Data.Text (Text)
import qualified Data.Text.IO as TIO
import System.IO (hFlush, stderr)

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
silentTracer :: (LogEvent -> IO ())
silentTracer _ = pure ()

-- | A tracer that prints all events to stderr.
verboseTracer :: (LogEvent -> IO ())
verboseTracer event = do
  TIO.hPutStrLn stderr (formatEvent event)
  hFlush stderr

-- | Format a log event for display.
formatEvent :: LogEvent -> Text
formatEvent (LogInfo msg)    = "[info] " <> msg
formatEvent (LogDebug msg)   = "[debug] " <> msg
formatEvent (LogWarning msg) = "[warning] " <> msg
