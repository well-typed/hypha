{-# LANGUAGE LambdaCase #-}
module Hypha.Prelude
  ( version
  , warnOnLeft
  ) where

import Data.Text (Text)
import Data.Text qualified as Text
import System.IO (hPutStrLn, stderr)

version :: String
version = "0.1.0"

-- | Run an 'IO' action returning 'Either'; on 'Left', emit a single
-- warning line to @stderr@ and substitute the supplied fallback.  Use
-- at the seams where degraded behaviour is intentional but the
-- underlying failure MUST be visible to the user.  See the
-- "Well-Typed Ethos" entry in @CLAUDE.md@: silent error-branch swallow
-- is banished.
warnOnLeft
  :: (err -> Text)   -- ^ render the error for the warning line
  -> a               -- ^ fallback value substituted on 'Left'
  -> IO (Either err a)
  -> IO a
warnOnLeft renderErr fallback action = action >>= \case
  Right x  -> pure x
  Left err -> do
    hPutStrLn stderr ("warning: " <> Text.unpack (renderErr err))
    pure fallback
