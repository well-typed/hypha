{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Hypha.Command.Doctor
  ( -- * Field sets
    compactKeys
  , fullKeys
    -- * Execution
  , runDoctor
  ) where

import Control.Exception (try, SomeException)
import Data.Aeson (Value, (.=))
import qualified Data.Aeson as Aeson
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory (doesFileExist, findExecutable)

import Hypha.Error (HyphaError (..))
import Hypha.Output.Outcome
  ( Outcome (..)
  )
import Hypha.Types.BuildPlan (BuildPlan)

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
runDoctor :: BuildPlan -> IO (Either HyphaError (Outcome Value))
runDoctor _plan = do
  ghcResult  <- checkGhc
  haddockResult <- checkHaddock
  planResult <- checkPlanJson

  let allPass = case [ghcResult, haddockResult, planResult] of
        [] -> True
        rs -> and [r == CheckPass "" | r <- rs]

      checks = Aeson.object
        [ "ghc"       .= checkToJson ghcResult
        , "haddock"   .= checkToJson haddockResult
        , "plan_json" .= checkToJson planResult
        ]

      outcome = OutcomeSuccess body allPass [] mempty mempty
        where
          body = Aeson.object
            [ "checks"   .= checks
            , "all_pass" .= allPass
            ]

  pure (Right outcome)

checkGhc :: IO CheckResult
checkGhc = do
  eGhc <- try @SomeException (findExecutable "ghc")
  case eGhc of
    Left _ -> pure (CheckFail "ghc not found on PATH")
    Right Nothing -> pure (CheckFail "ghc not found on PATH")
    Right (Just path) -> pure (CheckPass ("ghc found at " <> Text.pack path))

checkHaddock :: IO CheckResult
checkHaddock = do
  eHaddock <- try @SomeException (findExecutable "haddock")
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
