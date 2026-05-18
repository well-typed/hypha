{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Error
  ( HyphaError (..)
  , errorCode
  , errorMessage
  , errorExitCode
  , toOutcomeError
  ) where

import Data.Text (Text)

import Hypha.Exit (ExitCode, exitUserError, exitNotFound, exitNetworkError, exitCacheError, exitEnvironmentError, unExitCode)

-- | Typed errors produced by hypha.  Each constructor maps to exactly one
-- 'ExitCode' (totality verified by a property test in
-- @test/Property/Errors.hs@).
data HyphaError
  = UserError     !Text   -- ^ bad CLI args, malformed path, conflicting flags
  | NotFound      !Text   -- ^ symbol/pkg absent from the full fallback chain (plan → store → Hackage)
  | NetworkError  !Text   -- ^ --offline with cache miss, 429, 503, etc.
  | Corruption    !Text   -- ^ cache / parse / on-disk corruption
  | EnvError      !Text   -- ^ no plan.json, missing ghc/haddock, store unreachable, Stack
  deriving stock (Show, Eq)

errorCode :: HyphaError -> Text
errorCode = \case
  UserError    _ -> "USER_ERROR"
  NotFound     _ -> "NOT_FOUND"
  NetworkError _ -> "NETWORK_ERROR"
  Corruption   _ -> "CORRUPTION"
  EnvError     _ -> "ENV_ERROR"

errorMessage :: HyphaError -> Text
errorMessage = \case
  UserError    msg -> msg
  NotFound     msg -> msg
  NetworkError msg -> msg
  Corruption   msg -> msg
  EnvError     msg -> msg

-- | Total mapping from error to exit code.  Lives in 'Hypha.Exit'; this
-- module is the only place that decides which code each error uses.
errorExitCode :: HyphaError -> ExitCode
errorExitCode = \case
  UserError    _ -> exitUserError
  NotFound     _ -> exitNotFound
  NetworkError _ -> exitNetworkError
  Corruption   _ -> exitCacheError
  EnvError     _ -> exitEnvironmentError

-- | Convert a 'HyphaError' into the wire-format 'OutcomeError' fields.
toOutcomeError :: HyphaError -> (Text, Text, Int)
toOutcomeError e = (errorCode e, errorMessage e, unExitCode (errorExitCode e))
