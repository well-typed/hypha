{-# LANGUAGE OverloadedStrings #-}
-- | Golden coverage of the @hypha lookup@ outcome envelope.  Each
-- golden is produced by feeding a canned 'buildOutcome' through the
-- standard encoder; no real cache or Hoogle DB is touched.  This
-- means the goldens are stable regardless of network / filesystem
-- state, while still snapshotting the JSON shape an agent would
-- observe.
module Golden.Lookup (tests) where

import Data.Aeson (Value)
import qualified Data.Set as Set
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)

import Hypha.Cli.Types (ClientCommandTag (..), CommandTag (..))
import Hypha.Command.Lookup
  ( Provider (..), RemoteTierOutcome (..), buildOutcome )
import Hypha.Error (HyphaError)
import Hypha.Hoogle.Remote (RemoteError (..))
import Hypha.Hoogle.Tier (Tier (..))
import Hypha.Hoogle.Type (HoogleQuery (..))
import Hypha.Output.Json
  ( EnvelopeOpts (..), encodeEnvelopeValue, encodeErrorEnvelope
  , encodeOutcomeBytes )
import Hypha.Output.Outcome (Outcome)

tests :: TestTree
tests = testGroup "Golden.Lookup"
  [ goldenCase "lookup-cache-hit"         cacheHitOutcome
  , goldenCase "lookup-local-hoogle-hit"  localHitOutcome
  , goldenCase "lookup-remote-hit"        remoteHitOutcome
  , goldenCase "lookup-all-miss"          missOutcome
  , goldenCase "lookup-remote-error"      remoteErrorOutcome
  , goldenCase "lookup-offline"           offlineOutcome
  ]
  where
    goldenCase name outcome = goldenVsString name (goldPath name)
                                                  (pure (encode outcome))
    goldPath n = "test" </> "Golden" </> "golden" </> (n <> ".compact.json")
    encode = either
      (encodeEnvelopeValue envOpts . encodeErrorEnvelope (ClientTag LookupCmd))
      (encodeOutcomeBytes envOpts LookupCmd
        (Set.fromList ["query", "providers"])
        (Set.fromList ["query", "providers", "tiers_consulted"]))
    envOpts = EnvelopeOpts False [] False

mkProvider :: Tier -> Provider
mkProvider t = Provider "containers" "Data.Map" "lookup"
                 "Ord k => k -> Map k a -> Maybe a" t

cacheHitOutcome :: Either HyphaError (Outcome Value)
cacheHitOutcome =
  buildOutcome (HoogleQuery "lookup") [mkProvider TierCache]
               [TierCache] RemoteNotConsulted

localHitOutcome :: Either HyphaError (Outcome Value)
localHitOutcome =
  buildOutcome (HoogleQuery "lookup")
    [mkProvider TierLocalHoogle]
    [TierCache, TierLocalHoogle] RemoteNotConsulted

remoteHitOutcome :: Either HyphaError (Outcome Value)
remoteHitOutcome =
  buildOutcome (HoogleQuery "lookup")
    [mkProvider TierRemoteHoogle]
    [TierCache, TierLocalHoogle, TierRemoteHoogle] RemoteNotConsulted

missOutcome :: Either HyphaError (Outcome Value)
missOutcome = buildOutcome (HoogleQuery "doesNotExist") []
  [TierCache, TierLocalHoogle, TierRemoteHoogle] RemoteEmpty

remoteErrorOutcome :: Either HyphaError (Outcome Value)
remoteErrorOutcome = buildOutcome (HoogleQuery "x") []
  [TierCache, TierLocalHoogle, TierRemoteHoogle]
  (RemoteFailed (RemoteHttp "timeout after 10s"))

offlineOutcome :: Either HyphaError (Outcome Value)
offlineOutcome = buildOutcome (HoogleQuery "x") []
  [TierCache, TierLocalHoogle] RemoteSkippedOffline
