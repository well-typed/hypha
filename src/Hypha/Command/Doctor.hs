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
import Data.Aeson qualified as Aeson
import Data.Aeson (Value, (.=))
import Data.Set qualified as Set
import Data.Set (Set)
import Data.Text qualified as Text
import Data.Text (Text)
import Hypha.Cli.Types
import Hypha.Output.Outcome (Outcome, successOutcome)
import System.Directory (doesFileExist, findExecutable)

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

  let allPass = and [case r of CheckPass _ -> True; _ -> False | r <- [ghcResult, haddockResult, planResult]]
      checks = Aeson.object
        [ "ghc"       .= checkToJson ghcResult
        , "haddock"   .= checkToJson haddockResult
        , "plan_json" .= checkToJson planResult
        ]
      body = Aeson.object
        [ "checks"   .= checks
        , "all_pass" .= allPass
        ]
  -- Built through 'successOutcome' rather than the positional
  -- constructor: 'Outcome' takes a 'Bool' third, and spelling this out
  -- as @Outcome body DoctorCmd allPass [] mempty@ silently filed
  -- 'allPass' as @outside_plan@, so a healthy doctor reported itself
  -- outside a plan while its own plan_json check said otherwise.
  pure (successOutcome DoctorCmd body)

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
