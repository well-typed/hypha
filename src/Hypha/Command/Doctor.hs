{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Environment health check command.
--
-- The @doctor@ command diagnoses the Haskell development environment by checking:
--   * GHC availability on PATH
--   * Haddock availability on PATH
--   * Presence of dist-newstyle/cache/plan.json
--
-- Returns a JSON envelope with check statuses (pass/warn/fail) and an
-- @all_pass@ boolean summarizing the overall health.
module Hypha.Command.Doctor
  ( -- * Field sets
    compactKeys
  , fullKeys
    -- * Execution
  , runDoctor
  ) where

import Control.Exception.Safe (try, SomeException)
import Data.Aeson (Value, (.=))
import qualified Data.Aeson as Aeson
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory (doesFileExist, findExecutable)

import Hypha.Output.Outcome (Outcome (..))

compactKeys, fullKeys :: Set Text
compactKeys = Set.fromList ["checks", "all_pass"]
fullKeys    = compactKeys

-- | Result of a single health check.
data CheckResult
  = CheckPass !Text
  | CheckWarn !Text
  | CheckFail !Text
  deriving stock (Show, Eq)

-- | Run all environment checks.
--
-- Note: This returns 'Outcome Value' directly (not 'Either HyphaError')
-- because the checks themselves never fail at the IO boundary; they always
-- produce a structured result (pass/warn/fail) in the outcome body.
runDoctor :: IO (Outcome Value)
runDoctor = do
  ghcResult <- checkGhc
  haddockResult <- checkHaddock
  planResult <- checkPlanJson

  let allPass = case [ghcResult, haddockResult, planResult] of
        [] -> True
        rs -> and [case r of CheckPass _ -> True; _ -> False | r <- rs]
      checks = Aeson.object
        [ "ghc"       .= checkToJson ghcResult
        , "haddock"   .= checkToJson haddockResult
        , "plan_json" .= checkToJson planResult
        ]
      body = Aeson.object
        [ "checks"   .= checks
        , "all_pass" .= allPass
        ]
  pure $ Outcome body allPass [] mempty

checkGhc :: IO CheckResult
checkGhc = do
  eGhc <- try @IO @SomeException (findExecutable "ghc")
  case eGhc of
    Left _ -> pure (CheckFail "ghc not found on PATH")
    Right Nothing -> pure (CheckFail "ghc not found on PATH")
    Right (Just path) -> pure (CheckPass ("ghc found at " <> Text.pack path))

checkHaddock :: IO CheckResult
checkHaddock = do
  eHaddock <- try @IO @SomeException (findExecutable "haddock")
  case eHaddock of
    Left _ -> pure (CheckWarn "haddock not found on PATH (documentation generation unavailable)")
    Right Nothing -> pure (CheckWarn "haddock not found on PATH (documentation generation unavailable)")
    Right (Just path) -> pure (CheckPass ("haddock found at " <> Text.pack path))

checkPlanJson :: IO CheckResult
checkPlanJson = do
  exists <- doesFileExist "dist-newstyle/cache/plan.json"
  if exists
    then pure (CheckPass "plan.json found")
    else pure (CheckFail "plan.json not found (run `cabal build --dry-run`)")

checkToJson :: CheckResult -> Value
checkToJson = \case
  CheckPass msg -> Aeson.object
    [ "status" .= ("pass" :: Text)
    , "message" .= msg
    ]
  CheckWarn msg -> Aeson.object
    [ "status" .= ("warn" :: Text)
    , "message" .= msg
    ]
  CheckFail msg -> Aeson.object
    [ "status" .= ("fail" :: Text)
    , "message" .= msg
    ]
