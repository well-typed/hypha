{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
-- | The @hypha lookup@ command: tiered symbol resolution.
--
-- Tier order is fixed and short-circuiting:
--
--   1. 'Hypha.Search.PackageCache.lookupByName' — SQLite name index.
--   2. 'Hypha.Hoogle.Local.searchLocal'         — project Hoogle DB.
--   3. 'Hypha.Hoogle.Remote.searchRemote'       — hoogle.haskell.org.
--
-- This module owns the outcome assembly only; the individual tiers
-- live in their own modules so they can be tested independently.
module Hypha.Command.Lookup
  ( -- * Types
    LookupResult (..)
  , Provider (..)
  , Tier (..)
  , LookupOptions (..)
    -- * Cascade
  , runLookup
    -- * Pure helpers used by tests
  , chooseTiers
  , buildOutcome
    -- * JSON
  , providerToJSON
  , lookupResultToJSON
  ) where

import Data.Aeson (Value, (.=), object)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text

import Hypha.Hoogle.Local (HyphaHoogle, searchLocal)
import Hypha.Hoogle.Remote
  ( RemoteError (..), RemoteOptions, searchRemote )
import Hypha.Hoogle.Type (HoogleHit (..), HoogleQuery (..))
import Hypha.Output.Outcome
  ( Outcome (..), OutcomeError (..), Related (..) )
import Hypha.Search.PackageCache (HyphaPackageCache, lookupByName)

-- | Which tier produced a 'Provider'.
data Tier = TierCache | TierLocalHoogle | TierRemoteHoogle
  deriving stock (Show, Eq, Ord)

-- | A single hit, tagged with its origin tier.
data Provider = Provider
  { pPkg  :: !Text
  , pMod  :: !Text
  , pName :: !Text
  , pSig  :: !Text
  , pTier :: !Tier
  }
  deriving stock (Show, Eq)

-- | Result body of a successful 'runLookup'.
data LookupResult = LookupResult
  { lrQuery          :: !Text
  , lrProviders      :: ![Provider]
  , lrTiersConsulted :: ![Tier]
  }
  deriving stock (Show, Eq)

data LookupOptions = LookupOptions
  { loOffline :: !Bool
  , loRemote  :: !RemoteOptions
  }

-- | Top-level entry point.  Runs the cascade and assembles an
-- 'Outcome' for the CLI envelope encoder.
runLookup
  :: HyphaPackageCache
  -> HyphaHoogle
  -> LookupOptions
  -> Text
  -> IO (Outcome Value)
runLookup cache hoogleLocal opts q = do
  -- Tier 1
  cacheHits <- lookupByName cache q
  case cacheHits of
    (_:_) -> pure (buildOutcome q
                    (map (toProvider TierCache) cacheHits)
                    [TierCache] Nothing)
    [] -> do
      -- Tier 2
      localHits <- searchLocal hoogleLocal (HoogleQuery q)
      case localHits of
        (_:_) -> pure (buildOutcome q
                        (map (hitProvider TierLocalHoogle) localHits)
                        [TierCache, TierLocalHoogle] Nothing)
        [] -> do
          -- Tier 3
          remote <- searchRemote (loRemote opts) cache (HoogleQuery q)
          let tiersWithRemote =
                [TierCache, TierLocalHoogle, TierRemoteHoogle]
          case remote of
            Right hits | not (null hits) ->
              pure (buildOutcome q
                     (map (hitProvider TierRemoteHoogle) hits)
                     tiersWithRemote Nothing)
            Right _ ->
              pure (buildOutcome q [] tiersWithRemote
                     (Just (RemoteHttp "NOT_FOUND")))
              -- We reuse RemoteHttp here as a marker; buildOutcome
              -- maps Nothing-providers-+-empty-error to NOT_FOUND.
            Left RemoteOffline ->
              pure (buildOutcome q []
                     [TierCache, TierLocalHoogle]
                     (Just RemoteOffline))
            Left e ->
              pure (buildOutcome q [] tiersWithRemote (Just e))

-- | Pure outcome assembly: drives every shape (success, NOT_FOUND,
-- HOOGLE_OFFLINE, HOOGLE_REMOTE_ERROR) so property tests can poke at
-- it without IO.
buildOutcome
  :: Text                  -- ^ query
  -> [Provider]            -- ^ providers (may be empty on failure)
  -> [Tier]                -- ^ tiers actually consulted
  -> Maybe RemoteError     -- ^ remote-tier outcome when relevant
  -> Outcome Value
buildOutcome q providers tiers mErr =
  case (providers, mErr) of
    (_:_, _) ->
      OutcomeSuccess
        (lookupResultToJSON (LookupResult q providers tiers))
        False [] mempty
        [ Related (pPkg p <> "/" <> pMod p)
                  ( "hypha symbol "
                    <> pPkg p <> "/" <> pMod p <> "/" <> pName p )
        | p <- take 5 providers
        ]

    ([], Just RemoteOffline) ->
      OutcomeFailure
        (OutcomeError "HOOGLE_OFFLINE"
          "--offline (or HYPHA_OFFLINE) suppresses remote tier" 4)
        (Map.fromList
           [ ("retry_online", "hypha lookup " <> q)
           , ("query", q)
           , ("tiers_consulted", renderTiers tiers) ])

    ([], Just (RemoteHttp "NOT_FOUND")) ->
      OutcomeFailure
        (OutcomeError "NOT_FOUND" "no providers found" 3)
        (Map.fromList
           [ ("retry_with_prefix", "hypha lookup " <> q <> "*")
           , ("query", q)
           , ("tiers_consulted", renderTiers tiers) ])

    ([], Just e) ->
      OutcomeFailure
        (OutcomeError "HOOGLE_REMOTE_ERROR" (Text.pack (show e)) 5)
        (Map.fromList
           [ ("retry_offline", "hypha lookup " <> q <> " --offline")
           , ("raise_timeout",
                "HYPHA_HOOGLE_TIMEOUT=30 hypha lookup " <> q)
           , ("query", q)
           , ("tiers_consulted", renderTiers tiers) ])

    ([], Nothing) ->
      OutcomeFailure
        (OutcomeError "NOT_FOUND" "no providers found" 3)
        (Map.fromList
           [ ("retry_with_prefix", "hypha lookup " <> q <> "*")
           , ("query", q)
           , ("tiers_consulted", renderTiers tiers) ])

renderTiers :: [Tier] -> Text
renderTiers = Text.intercalate "," . map tierToText

-- | Pure tier-prefix model used by property tests.  Mirrors the
-- short-circuit logic of 'runLookup' without performing IO.
chooseTiers
  :: Bool   -- ^ Tier 1 hit?
  -> Bool   -- ^ Tier 2 hit?
  -> Bool   -- ^ offline?
  -> Bool   -- ^ Tier 3 hit? (unused when offline)
  -> [Tier]
chooseTiers t1 t2 offline _t3
  | t1        = [TierCache]
  | t2        = [TierCache, TierLocalHoogle]
  | offline   = [TierCache, TierLocalHoogle]
  | otherwise = [TierCache, TierLocalHoogle, TierRemoteHoogle]

-- Adapters --------------------------------------------------------------

toProvider :: Tier -> (Text, Text, Text, Text) -> Provider
toProvider t (pkg, modT, name, sig) = Provider pkg modT name sig t

hitProvider :: Tier -> HoogleHit -> Provider
hitProvider t h = Provider (hhPackage h) (hhModule h) (hhName h) (hhSig h) t

-- JSON ------------------------------------------------------------------

tierToText :: Tier -> Text
tierToText = \case
  TierCache         -> "cache"
  TierLocalHoogle   -> "local-hoogle"
  TierRemoteHoogle  -> "remote-hoogle"

providerToJSON :: Provider -> Value
providerToJSON p = object
  [ "pkg"  .= pPkg p
  , "mod"  .= pMod p
  , "name" .= pName p
  , "sig"  .= pSig p
  , "tier" .= tierToText (pTier p)
  ]

lookupResultToJSON :: LookupResult -> Value
lookupResultToJSON r = object
  [ "query"           .= lrQuery r
  , "providers"       .= map providerToJSON (lrProviders r)
  , "tiers_consulted" .= map tierToText (lrTiersConsulted r)
  ]

