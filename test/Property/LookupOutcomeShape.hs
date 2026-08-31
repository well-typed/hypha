{-# LANGUAGE OverloadedStrings #-}
module Property.LookupOutcomeShape (tests) where

import qualified Data.Map.Strict as Map
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import Hypha.Command.Lookup
  ( Provider (..), buildOutcome, RemoteTierOutcome (..) )
import Hypha.Error (HyphaError, errorActions, errorCode)
import Hypha.Hoogle.Remote (RemoteError (..))
import Hypha.Hoogle.Tier (Tier (..))
import Hypha.Hoogle.Type (HoogleQuery (..))
import Hypha.Types.PackageId (Version (..))
import Hypha.Output.Outcome (Outcome (..))

mkProvider :: Provider
mkProvider =
  Provider "containers" "Data.Map" "lookup" "sig" TierCache
           (Just (Version "0.6.7"))

expectFailure :: Either HyphaError (Outcome a) -> (HyphaError -> IO ()) -> IO ()
expectFailure r k = case r of
  Left err -> k err
  Right _  -> fail "expected failure"

expectSuccess :: Either HyphaError (Outcome a) -> (Outcome a -> IO ()) -> IO ()
expectSuccess r k = case r of
  Right oc -> k oc
  Left _   -> fail "expected success"

tests :: TestTree
tests = testGroup "Property.LookupOutcomeShape"
  [ testCase "non-empty providers => Right Outcome with action hints" $
      expectSuccess
        (buildOutcome (HoogleQuery "lookup") [mkProvider]
                      [TierCache] RemoteNotConsulted) $ \oc ->
          assertBool "has actions" (not (Map.null (outcomeActions oc)))

  , testCase "empty providers + offline => HOOGLE_OFFLINE failure" $
      expectFailure
        (buildOutcome (HoogleQuery "x") []
                      [TierCache, TierLocalHoogle] RemoteSkippedOffline) $ \err -> do
          errorCode err @?= "HOOGLE_OFFLINE"
          assertBool "has actions" (not (Map.null (errorActions err)))

  , testCase "empty providers + remote http error => HOOGLE_REMOTE_ERROR" $
      expectFailure
        (buildOutcome (HoogleQuery "x") []
                      [TierCache, TierLocalHoogle, TierRemoteHoogle]
                      (RemoteFailed (RemoteHttp "boom"))) $ \err -> do
          errorCode err @?= "HOOGLE_REMOTE_ERROR"
          assertBool "carries retry_offline"
            (Map.member "retry_offline" (errorActions err))

  , testCase "suggested commands quote the query" $
      expectFailure
        (buildOutcome (HoogleQuery "Ord b => (a -> b) -> [a] -> [a]") []
                      [TierCache, TierLocalHoogle, TierRemoteHoogle]
                      (RemoteFailed (RemoteHttp "boom"))) $ \err -> do
          -- Unquoted, "=>" makes the shell truncate the file the
          -- suggestion tells the user to run.
          Map.lookup "retry_offline" (errorActions err)
            @?= Just "hypha lookup 'Ord b => (a -> b) -> [a] -> [a]' --offline"
          Map.lookup "raise_timeout" (errorActions err)
            @?= Just "hypha lookup 'Ord b => (a -> b) -> [a] -> [a]' \
                     \--hoogle-timeout 30"
          -- The query itself stays raw: it is data, not a command.
          Map.lookup "query" (errorActions err)
            @?= Just "Ord b => (a -> b) -> [a] -> [a]"

  , testCase "the prefix suggestion is not a glob" $
      expectFailure
        (buildOutcome (HoogleQuery "sortOn") []
                      [TierCache, TierLocalHoogle, TierRemoteHoogle]
                      RemoteEmpty) $ \err ->
          Map.lookup "retry_with_prefix" (errorActions err)
            @?= Just "hypha lookup 'sortOn*'"

  , testCase "empty providers, remote consulted but empty => NOT_FOUND" $
      expectFailure
        (buildOutcome (HoogleQuery "x") []
                      [TierCache, TierLocalHoogle, TierRemoteHoogle]
                      RemoteEmpty) $ \err ->
          errorCode err @?= "NOT_FOUND"
  ]
