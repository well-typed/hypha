{-# LANGUAGE DerivingStrategies #-}
module Hypha.Exit
  ( -- * Types
    ExitCode (..)
    -- * Standard exit codes
  , exitOk
  , exitUserError
  , exitNotFound
  , exitNetworkError
  , exitCacheError
  , exitEnvironmentError
  , exitToolMissing
  , exitInternalError
    -- * Conversion
  , toSystemExitCode
  ) where

import qualified System.Exit as System

-- | Typed exit codes for hypha.
--
--   Each code has a specific meaning documented in the design spec.
newtype ExitCode = ExitCode { unExitCode :: Int }
  deriving stock (Show, Eq, Ord)

-- | Successful completion.
exitOk :: ExitCode
exitOk = ExitCode 0

-- | User error — bad CLI args, malformed path, conflicting flags.
exitUserError :: ExitCode
exitUserError = ExitCode 2

-- | Not found — symbol/pkg absent from the full fallback chain (plan → store → Hackage).
exitNotFound :: ExitCode
exitNotFound = ExitCode 3

-- | Network error — including @--offline@ with cache miss.
exitNetworkError :: ExitCode
exitNetworkError = ExitCode 4

-- | Cache / parse / on-disk corruption.
exitCacheError :: ExitCode
exitCacheError = ExitCode 5

-- | Environment error — no plan.json, missing ghc/haddock, store unreachable, Stack.
exitEnvironmentError :: ExitCode
exitEnvironmentError = ExitCode 7

-- | Tool missing — a required external binary (haddock, cabal, ghc) was
-- not found on @PATH@.  Distinct from 'exitEnvironmentError' because it
-- signals to agents that the failure is recoverable by installing or
-- exposing the tool, not by reconfiguring the project.
exitToolMissing :: ExitCode
exitToolMissing = ExitCode 8

-- | Internal error — an exception escaped the library and was caught
-- by the top-level @catchAny@ in @app/hypha/Main.hs@.  Distinct from
-- every other code because it signals a /bug or genuinely unhandled
-- environment failure/ in hypha itself, not a user- or input-level
-- problem.
exitInternalError :: ExitCode
exitInternalError = ExitCode 9

-- | Convert our typed 'ExitCode' to 'System.Exit.ExitCode'.
toSystemExitCode :: ExitCode -> System.ExitCode
toSystemExitCode (ExitCode n) =
  if n == 0
    then System.ExitSuccess
    else System.ExitFailure n
