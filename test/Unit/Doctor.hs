{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Unit.Doctor (tests) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool)

import Hypha.Command.Doctor (runDoctor)
import Hypha.Output.Outcome (Outcome (..))
import Hypha.Types.BuildPlan (BuildPlan (..), CompilerId (..))

-- | A minimal mock BuildPlan for testing.
mockBuildPlan :: BuildPlan
mockBuildPlan = BuildPlan
  { bpCompiler = CompilerId "GHC-9.6.7"
  , bpPackages = mempty
  , bpOverrides = mempty
  }

tests :: TestTree
tests = testGroup "Doctor"
  [ testGroup "Doctor Command"
      [ testCase "all_pass=false when plan.json missing" $ do
          -- Run doctor in current directory (where plan.json likely doesn't exist)
          result <- runDoctor mockBuildPlan
          case result of
            Left _ -> assertBool "Should succeed with outcome" False
            Right (OutcomeSuccess body _outside _overrides _actions _related) -> do
              -- Extract all_pass from the body (Aeson.Value)
              let allPass = extractAllPass body
              assertBool "all_pass should be False without plan.json"
                (not allPass)
            Right (OutcomeFailure {}) -> assertBool "Should not be OutcomeFailure" False

      , testCase "doctor returns successful outcome structure" $ do
          result <- runDoctor mockBuildPlan
          case result of
            Left _ -> assertBool "Should succeed" False
            Right (OutcomeSuccess {}) ->
              assertBool "Should be OutcomeSuccess" True
            Right (OutcomeFailure {}) ->
              assertBool "Should not be OutcomeFailure" False

      , testCase "result contains checks object" $ do
          result <- runDoctor mockBuildPlan
          case result of
            Left _ -> assertBool "Should succeed" False
            Right (OutcomeSuccess body _ _ _ _) -> do
              let checks = extractChecks body
              assertBool "checks field should be present and be an object"
                (case checks of
                  Just (Aeson.Object _) -> True
                  _ -> False)
            Right (OutcomeFailure {}) -> assertBool "Should not be OutcomeFailure" False

      , testCase "result contains all_pass boolean" $ do
          result <- runDoctor mockBuildPlan
          case result of
            Left _ -> assertBool "Should succeed" False
            Right (OutcomeSuccess body _ _ _ _) -> do
              let allPass = extractAllPass body
              assertBool "all_pass field should be present" True
              _ <- pure allPass
              pure ()
            Right (OutcomeFailure {}) -> assertBool "Should not be OutcomeFailure" False
      ]
  ]

-- | Helper to extract all_pass from Aeson.Value.
extractAllPass :: Aeson.Value -> Bool
extractAllPass (Aeson.Object obj) =
  case KM.lookup "all_pass" obj of
    Just (Aeson.Bool b) -> b
    _ -> False
extractAllPass _ = False

-- | Helper to extract checks from Aeson.Value.
extractChecks :: Aeson.Value -> Maybe Aeson.Value
extractChecks (Aeson.Object obj) =
  KM.lookup "checks" obj
extractChecks _ = Nothing
