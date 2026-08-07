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
  , LookupOptions (..)
  , RemoteTierOutcome (..)
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
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Hypha.Cli.Types
import Hypha.Error (HyphaError (..))
import Hypha.Hoogle.Local (HyphaHoogle, searchLocal)
import Hypha.Hoogle.Remote ( RemoteError (..), RemoteOptions, searchRemote )
import Hypha.Hoogle.Tier (Tier (..), tierLabel)
import Hypha.Hoogle.Type (HoogleHit (..), HoogleQuery (..))
import Hypha.Output.Outcome (Outcome (..))
import Hypha.Search.Cache (CacheScope, VersionedRow (..))
import Hypha.Search.Index (IndexRow (..))
import Hypha.Search.PackageCache (HyphaPackageCache, lookupByName)
import Hypha.Types.ComponentName (ComponentKey (..))
import Hypha.Types.PackageId (Version (..))
import Hypha.Types.SymbolPath (ModulePath (..), Signature (..), SymbolName (..))

-- | A single hit, tagged with its origin tier.
data Provider = Provider
  { pPkg     :: !Text
  , pMod     :: !Text
  , pName    :: !Text
  , pSig     :: !Text
  , pTier    :: !Tier
  , pVersion :: !(Maybe Version)
    -- ^ The version the row was indexed under.  'Just' on the cache
    -- tier, which reads a @(pkg, version)@ entry and so always knows;
    -- 'Nothing' on the Hoogle tiers, which answer with a package name
    -- and no version.  Without it two rows for the same symbol in the
    -- same module of the same package — the shape a cache holding two
    -- versions produces — render identically and read as a duplicate.
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
  { loOffline      :: !Bool
  , loCacheScope   :: !CacheScope
    -- ^ Which of the shared cache's versions tier 1 may answer from.
    -- The DB is keyed on @(package, version)@ and shared across every
    -- project on the host, so left unscoped it answers from the host's
    -- indexing history rather than from this project's plan.
  , loRemote       :: !RemoteOptions
  , loPrepareLocal :: !(IO ())
    -- ^ Action invoked /just-in-time/ before tier 2 ('searchLocal').
    -- Used to bring the project's local Hoogle DB up to date.  Hoisted
    -- into 'LookupOptions' so the cascade can skip the prep work
    -- entirely when tier 1 hits: the package cache lookup is a single
    -- SQLite read, but ensuring the Hoogle DB freshness involves
    -- enumerating units and re-indexing stale ones, which is
    -- expensive and noisy.  Defaulting to @pure ()@ keeps the cascade
    -- usable from test harnesses that don't care about tier 2.
  }

-- | What happened on the remote tier, when the earlier tiers were empty
-- and the cascade actually reached it.  Used by 'buildOutcome' so we
-- never have to overload a 'RemoteError' constructor with a sentinel
-- value (the prior @RemoteHttp \"NOT_FOUND\"@ trick was banished).
data RemoteTierOutcome
    -- | Earlier tier hit; remote tier never consulted.
  = RemoteNotConsulted
    -- | @--offline@ suppressed the remote call.
  | RemoteSkippedOffline
    -- | Remote returned with zero hits.
  | RemoteEmpty
    -- | Remote call failed with the embedded structured error.
  | RemoteFailed !RemoteError
  deriving stock (Show, Eq)

-- | Top-level entry point.  Runs the cascade and assembles an
-- 'Outcome' for the CLI envelope encoder.
runLookup
  :: HyphaPackageCache
  -> HyphaHoogle
  -> LookupOptions
  -> HoogleQuery
  -> IO (Either HyphaError (Outcome Value))
runLookup cache hoogleLocal opts q@(HoogleQuery qText) = do
  -- Tier 1
  cacheHits <- lookupByName cache (loCacheScope opts) qText
  case cacheHits of
    (_:_) -> pure (buildOutcome q
                    (map (toProvider TierCache) cacheHits)
                    [TierCache] RemoteNotConsulted)
    [] -> do
      -- Tier 2: bring the local Hoogle DB up to date only now that we
      -- actually need it (tier 1 missed).
      loPrepareLocal opts
      localHits <- searchLocal hoogleLocal q
      case localHits of
        (_:_) -> pure (buildOutcome q
                        (map (hitProvider TierLocalHoogle) localHits)
                        [TierCache, TierLocalHoogle] RemoteNotConsulted)
        [] -> do
          -- Tier 3
          remote <- searchRemote (loRemote opts) cache q
          let tiersWithRemote =
                [TierCache, TierLocalHoogle, TierRemoteHoogle]
          case remote of
            Right hits | not (null hits) ->
              pure (buildOutcome q
                     (map (hitProvider TierRemoteHoogle) hits)
                     tiersWithRemote RemoteNotConsulted)
            Right _ ->
              pure (buildOutcome q [] tiersWithRemote RemoteEmpty)
            Left RemoteOffline ->
              pure (buildOutcome q []
                     [TierCache, TierLocalHoogle]
                     RemoteSkippedOffline)
            Left e ->
              pure (buildOutcome q [] tiersWithRemote (RemoteFailed e))

-- | Pure outcome assembly.  When providers are present we build a
-- success 'Outcome'; otherwise the failure is surfaced as a typed
-- 'HyphaError' carrying the original 'HoogleQuery', the @['Tier']@
-- consulted, and the structured 'RemoteError' (when relevant).  Wire
-- rendering (comma-joined tier list, etc.) belongs to
-- "Hypha.Error.errorActions" — not here.
buildOutcome
  :: HoogleQuery           -- ^ query (carried as a domain type)
  -> [Provider]            -- ^ providers (may be empty on failure)
  -> [Tier]                -- ^ tiers actually consulted
  -> RemoteTierOutcome     -- ^ what happened on the remote tier
  -> Either HyphaError (Outcome Value)
buildOutcome q providers tiers remoteOutcome =
  case (providers, remoteOutcome) of
    (_:_, _) -> Right $ Outcome
      { outcomeResult      = lookupResultToJSON
                               (LookupResult (unHoogleQuery q) providers tiers)
      , outcomeTag         = LookupCmd
      , outcomeOutsidePlan = False
      , outcomeOverrides   = []
      , outcomeActions     = Map.fromList
          [ ( pPkg p <> "/" <> pMod p <> "/" <> pName p
            , "hypha symbol "
                <> pPkg p <> "/" <> pMod p <> "/" <> pName p )
          | p <- take 5 providers
          ]
      }

    ([], RemoteSkippedOffline) -> Left (HoogleOffline      q tiers)
    ([], RemoteFailed e)       -> Left (HoogleRemoteError  q tiers e)
    ([], RemoteEmpty)          -> Left (HoogleNotFound     q tiers)
    -- Unreachable in practice (earlier tiers produced no providers
    -- so the cascade always consults remote), but kept total.
    ([], RemoteNotConsulted)   -> Left (HoogleNotFound     q tiers)

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

-- | A cached index row as a lookup provider.  The row's definition
-- module and visibility do not appear in @hypha lookup@'s output shape,
-- which reports where a symbol is /available/, not where it is declared.
-- Built with record syntax rather than positionally: 'Provider' now has
-- two adjacent fields the type checker cannot tell apart by shape, and
-- the same positional habit is what filed a doctor's @all_pass@ as its
-- @outside_plan@.
toProvider :: Tier -> VersionedRow -> Provider
toProvider t (VersionedRow v r) = Provider
  { pPkg     = unComponentKey (rowComponent r)
  , pMod     = unModulePath   (rowModule r)
  , pName    = unSymbolName   (rowName r)
  , pSig     = unSignature    (rowSignature r)
  , pTier    = t
  , pVersion = Just v
  }

hitProvider :: Tier -> HoogleHit -> Provider
hitProvider t h = Provider
  { pPkg     = hhPackage h
  , pMod     = hhModule h
  , pName    = hhName h
  , pSig     = hhSig h
  , pTier    = t
  , pVersion = Nothing
  }

-- JSON ------------------------------------------------------------------

providerToJSON :: Provider -> Value
providerToJSON p = object $
  [ "pkg"  .= pPkg p
  , "mod"  .= pMod p
  , "name" .= pName p
  , "sig"  .= pSig p
  , "tier" .= tierLabel (pTier p)
  ]
  -- Emitted only when known: a Hoogle hit carries a package name and no
  -- version, and a @version: null@ would read as "no version exists".
  <> [ "version" .= unVersion v | Just v <- [pVersion p] ]

lookupResultToJSON :: LookupResult -> Value
lookupResultToJSON r = object
  [ "query"           .= lrQuery r
  , "providers"       .= map providerToJSON (lrProviders r)
  , "tiers_consulted" .= map tierLabel (lrTiersConsulted r)
  ]
