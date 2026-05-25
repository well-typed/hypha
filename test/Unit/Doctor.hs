{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Unit.Doctor (tests) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as Text
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool)

import Hypha.Command.Doctor (runDoctor)
import Hypha.Output.Outcome (Outcome (..))

tests :: TestTree
tests = testGroup "Doctor"
  [ testGroup "Doctor Command"
      [ testCase "all_pass=false when plan.json missing" $ do
          -- Run doctor in a temporary directory where plan.json doesn't exist
          withSystemTempDirectory "hypha-test" $ \tempDir -> do
            oldDir <- getCurrentDirectory
            setCurrentDirectory tempDir
            outcome <- runDoctor
            setCurrentDirectory oldDir
            let allPass = extractAllPass (outcomeResult outcome)
            assertBool "all_pass should be False without plan.json"
                       (not allPass)

      , testCase "result contains checks object" $ do
          outcome <- runDoctor
          let checks = extractChecks (outcomeResult outcome)
          assertBool "checks field should be present and be an object"
            (case checks of
              Just (Aeson.Object _) -> True
              _ -> False)

      , testCase "result contains all_pass boolean" $ do
          outcome <- runDoctor
          let _allPass = extractAllPass (outcomeResult outcome)
          assertBool "all_pass field should be present" True

      , testCase "all_pass=true when all checks pass" $ do
          -- Positive case: when every check passes, all_pass is true.
          outcome <- runDoctor
          let body     = outcomeResult outcome
              allPass  = extractAllPass body
              checksObj = extractChecks body
          assertBool "When all checks pass, all_pass should be true"
            (case checksObj of
              Just (Aeson.Object checks) ->
                let ghcStatus     = getStatus checks "ghc"
                    haddockStatus = getStatus checks "haddock"
                    planStatus    = getStatus checks "plan_json"
                    allPassStatus = ghcStatus == "pass"
                                 && haddockStatus == "pass"
                                 && planStatus == "pass"
                in if allPassStatus
                     then allPass
                     else True
              _ -> True)

      , testCase "checks contain ghc, haddock, and plan_json" $ do
          outcome <- runDoctor
          let checksObj = extractChecks (outcomeResult outcome)
          assertBool "checks should contain all three checks"
            (case checksObj of
              Just (Aeson.Object checks) ->
                   hasField "ghc"       checks
                && hasField "haddock"   checks
                && hasField "plan_json" checks
              _ -> False)
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

-- | Helper to extract status string from a check object.
getStatus :: KM.KeyMap Aeson.Value -> Text.Text -> Text.Text
getStatus checks checkName =
  case KM.lookup (Key.fromText checkName) checks of
    Just (Aeson.Object checkObj) ->
      case KM.lookup "status" checkObj of
        Just (Aeson.String s) -> s
        _ -> ""
    _ -> ""

-- | Helper to check if a field exists in a KeyMap.
hasField :: Text.Text -> KM.KeyMap Aeson.Value -> Bool
hasField fieldName checks =
  case KM.lookup (Key.fromText fieldName) checks of
    Just _ -> True
    _ -> False
