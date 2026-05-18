{-# LANGUAGE DerivingStrategies #-}
module Hypha.Error
  ( -- * Types
    HyphaError (..)
    -- * Conversion
  , errorToExitCode
  , errorToMessage
  ) where

import Data.Text (Text)
import qualified Data.Text as Text

import Hypha.Exit (ExitCode (..), exitUserError, exitNotFound, exitNetworkError, exitCacheError, exitEnvironmentError)

-- | Sum type representing all possible hypha errors.
data HyphaError
  = CliError !Text
    -- ^ Bad CLI args, malformed path, conflicting flags.
  | NotFound !Text
    -- ^ Symbol/package not in plan (and no @--any@), or absent from Hackage.
  | NetworkError !Text
    -- ^ Network failure, including @--offline@ with cache miss.
  | CacheError !Text
    -- ^ Cache / parse / on-disk corruption.
  | EnvironmentError !Text
    -- ^ No plan.json, missing ghc/haddock, store unreachable, Stack.
  deriving stock (Show, Eq)

-- | Map an error to its corresponding exit code.
errorToExitCode :: HyphaError -> ExitCode
errorToExitCode (CliError _)          = exitUserError
errorToExitCode (NotFound _)          = exitNotFound
errorToExitCode (NetworkError _)      = exitNetworkError
errorToExitCode (CacheError _)        = exitCacheError
errorToExitCode (EnvironmentError _)  = exitEnvironmentError

-- | Extract a human-readable message from an error.
errorToMessage :: HyphaError -> Text
errorToMessage (CliError msg)         = msg
errorToMessage (NotFound msg)         = msg
errorToMessage (NetworkError msg)     = msg
errorToMessage (CacheError msg)       = msg
errorToMessage (EnvironmentError msg) = msg
