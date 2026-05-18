{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hypha.Error
  ( HyphaError (..)
  , ExitCode (..)
  , errorCode
  , errorMessage
  , errorExitCode
  , toOutcomeError
  ) where

import Data.Text (Text)
import qualified Data.Text as Text

-- | Typed errors produced by hypha.  Each constructor maps to exactly
-- one 'ExitCode' (totality verified by a property test).
data HyphaError
  = UserError     !Text   -- ^ bad CLI args, malformed path, conflicting flags
  | NotFound      !Text   -- ^ symbol/pkg not in plan (and no --any), or absent from Hackage
  | NetworkError  !Text   -- ^ --offline with cache miss, 429, 503, etc.
  | Corruption    !Text   -- ^ cache / parse / on-disk corruption
  | EnvError      !Text   -- ^ no plan.json, missing ghc/haddock, store unreachable, Stack
  deriving stock (Show, Eq)

-- | Typed exit codes.  We never use the raw 'System.Exit.ExitCode'.
newtype ExitCode = ExitCode { unExitCode :: Int }
  deriving stock   (Show, Eq, Ord)
  deriving newtype (Read)

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

errorExitCode :: HyphaError -> ExitCode
errorExitCode = \case
  UserError    _ -> ExitCode 2
  NotFound     _ -> ExitCode 3
  NetworkError _ -> ExitCode 4
  Corruption   _ -> ExitCode 5
  EnvError     _ -> ExitCode 7

-- | Convert a 'HyphaError' into the wire-format 'OutcomeError' fields.
toOutcomeError :: HyphaError -> (Text, Text, Int)
toOutcomeError e = (errorCode e, errorMessage e, unExitCode (errorExitCode e))
