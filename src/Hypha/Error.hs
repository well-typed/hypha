{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Error
  ( -- * Umbrella error
    HyphaError (..)
    -- * Typed sub-reasons
  , UserErrorReason (..)
  , renderUserErrorReason
  , NotFoundReason (..)
  , renderNotFoundReason
  , Tool (..)
  , renderTool
  , toolFromFilename
    -- * Classification
  , errorCode
  , errorMessage
  , errorExitCode
  , errorActions
    -- * ExceptT helpers
  , discoverProjectRootE
  , loadBuildPlanE
  ) where

import Control.Exception (IOException, SomeException, displayException)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Except (ExceptT (ExceptT))
import Data.Bifunctor (first)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text

import Hypha.Exit
  ( ExitCode, exitUserError, exitNotFound, exitNetworkError, exitCacheError
  , exitEnvironmentError, exitToolMissing )
import Hypha.Hackage.Api (HackageError, renderHackageError)
import qualified Hypha.Hackage.Api as Hackage
import Hypha.Hoogle.Remote (RemoteError, renderRemoteError)
import Hypha.Hoogle.Tier (Tier, renderTierList)
import Hypha.Hoogle.Type (HoogleQuery (..))
import Hypha.Project.Discovery (DiscoveryError (..), discoverProjectRoot)
import Hypha.Project.Overrides (OverrideError, renderOverrideError)
import Hypha.Project.Plan (PlanError (..), loadBuildPlan)
import Hypha.Server.Bind (BindError, renderBindError)
import Hypha.Types.BuildPlan (BuildPlan, ProjectRoot (..))
import Hypha.Types.PackageId
  ( PackageId (..), PackageName (..), Version (..) )

-- | Typed reasons a CLI invocation can be rejected as user error.  Each
-- variant captures the structured input that failed validation
-- (raw argument, 'OverrideError', 'BindError') — never a pre-rendered
-- message.  Rendering happens in 'renderUserErrorReason'.
data UserErrorReason
    -- | Expected @PKG/MOD@; got the embedded raw argument.
  = UserExpectedPkgMod          !Text
    -- | Expected @PKG/MOD[/SYM]@; got the embedded raw argument.
  | UserExpectedPkgModOptSym    !Text
    -- | Expected @PKG/MOD/SYM@; got the embedded raw argument.
  | UserExpectedSymbolPath      !Text
    -- | Symbol-path argument was missing its module segment.
  | UserSymbolPathMissingModule !Text
    -- | Symbol-path argument was missing its symbol segment.
  | UserSymbolPathMissingSymbol !Text
    -- | @--package-override@ failed to parse.
  | UserOverrideParse           !OverrideError
    -- | @--bind@ value rejected (malformed or non-loopback).
  | UserBindError               !BindError
  deriving stock (Show, Eq)

renderUserErrorReason :: UserErrorReason -> Text
renderUserErrorReason = \case
  UserExpectedPkgMod          arg -> "expected PKG/MOD (got: "       <> arg <> ")"
  UserExpectedPkgModOptSym    arg -> "expected PKG/MOD[/SYM] (got: " <> arg <> ")"
  UserExpectedSymbolPath      arg -> "expected PKG/MOD/SYM (got: "   <> arg <> ")"
  UserSymbolPathMissingModule arg ->
    "expected PKG/MOD/SYM — module segment missing (got: " <> arg <> ")"
  UserSymbolPathMissingSymbol arg ->
    "expected PKG/MOD/SYM — symbol segment missing (got: " <> arg <> ")"
  UserOverrideParse err -> renderOverrideError err
  UserBindError     err -> renderBindError err

-- | Typed reasons a lookup boundary returned no result.  Carries the
-- structured pid / module / symbol involved so the wire layer
-- renders the message uniformly and downstream consumers can still
-- pattern-match on the cause.
data NotFoundReason
    -- | Package not present in the resolved build plan.
  = NotFoundPackageInPlan    !PackageName
    -- | Package not present in the local @--offline@ cache.
  | NotFoundOfflineCache     !PackageName
    -- | Source directory not on disk for the resolved package.
  | NotFoundSourceDir        !PackageId
    -- | Package source could not be located (plan / store / Hackage).
  | NotFoundSource           !PackageId
    -- | Specific module file not found at a known location.
  | NotFoundModuleFile       !PackageId !Text !FilePath
    -- | Module file not found anywhere under a search directory.
  | NotFoundModuleFileUnder  !FilePath !Text
    -- | Symbol not found inside an otherwise-located module.
  | NotFoundSymbol           !PackageId !Text !Text
  deriving stock (Show, Eq)

renderNotFoundReason :: NotFoundReason -> Text
renderNotFoundReason = \case
  NotFoundPackageInPlan    n ->
    "package '" <> unPackageName n <> "' not in build plan"
  NotFoundOfflineCache     n ->
    "package '" <> unPackageName n
      <> "' not cached; can't fetch from Hackage in offline mode"
  NotFoundSourceDir        pid ->
    "source directory not found for " <> renderPid pid
  NotFoundSource           pid ->
    "source not found for " <> renderPid pid
      <> "; run `cabal build` first"
  NotFoundModuleFile       pid modPath path ->
    "module file not found for " <> renderPid pid
      <> "/" <> modPath <> ": " <> Text.pack path
  NotFoundModuleFileUnder  srcDir modPath ->
    "module file not found under " <> Text.pack srcDir
      <> " for " <> modPath
  NotFoundSymbol           pid modPath sym ->
    "symbol '" <> sym <> "' not found in " <> renderPid pid
      <> "/" <> modPath

renderPid :: PackageId -> Text
renderPid (PackageId (PackageName n) (Version v)) = n <> "-" <> v

-- | External binary hypha depends on.  Used by 'ToolMissing' to name
-- which executable was absent from @PATH@ when the cascade caught an
-- @ENOENT@.  'ToolUnknown' carries the filename so the message stays
-- informative even when we don't recognise the binary.
data Tool
  = ToolHaddock
  | ToolCabal
  | ToolGhc
  | ToolTar
  | ToolUnknown !Text
  deriving stock (Show, Eq)

renderTool :: Tool -> Text
renderTool = \case
  ToolHaddock     -> "haddock"
  ToolCabal       -> "cabal"
  ToolGhc         -> "ghc"
  ToolTar         -> "tar"
  ToolUnknown nm  -> nm

-- | Classify a binary name (typically lifted from 'ioe_filename') into
-- a known 'Tool'.  Falls back to 'ToolUnknown' so the classifier
-- remains total.
toolFromFilename :: Maybe FilePath -> Tool
toolFromFilename = \case
  Just "haddock" -> ToolHaddock
  Just "cabal"   -> ToolCabal
  Just "ghc"     -> ToolGhc
  Just "tar"     -> ToolTar
  Just other     -> ToolUnknown (Text.pack other)
  Nothing        -> ToolUnknown "<unknown>"

-- | Umbrella error type produced by hypha.  Every fallible boundary of
-- the CLI funnels through this ADT.  Constructors embed precise
-- sub-errors and any envelope context (query, tiers consulted, ...)
-- needed to render the wire-format error/actions without an auxiliary
-- type.
--
-- Each constructor maps to exactly one 'ExitCode' (see 'errorExitCode'
-- / 'errorCode'); 'HackageFailure' dispatches further based on the
-- embedded 'HackageError' variant so the wire code reflects the actual
-- cause (transport ↔ NETWORK_ERROR, decode/missing-field ↔ CORRUPTION,
-- offline-miss ↔ NOT_FOUND).
--
-- Per the project ethos (CLAUDE.md, \"Render at the edge\"): error
-- constructors carry domain types — 'UserErrorReason',
-- 'NotFoundReason', 'HoogleQuery', @['Tier']@, 'RemoteError',
-- 'HackageError', 'Tool', 'IOException', 'SomeException' — never
-- pre-rendered 'Text'.  Rendering happens in 'errorMessage' /
-- 'errorActions', at the wire boundary.
--
-- 'Eq' is intentionally not derived: 'SomeException' has no useful
-- structural equality and 'IOException' likewise.  Tests pattern-match
-- on constructors rather than comparing whole values.
data HyphaError
  = UserError         !UserErrorReason
  | NotFound          !NotFoundReason
  | HackageFailure    !PackageName !HackageError
    -- ^ Structured Hackage cause.  Dispatch on the variant for wire
    --   code / exit code mapping.
  | NetworkError      !SomeException
    -- ^ Catch-all transport bottom raised inside @hypha lookup@.
    --   Carries the originating exception so debug output / future
    --   structured matching is still possible; rendering happens at
    --   the wire layer via 'displayException'.
  | ToolMissing       !Tool !IOException
    -- ^ Required external binary was not on @PATH@.  Carries the
    --   recognised 'Tool' tag and the originating 'IOException'
    --   (typically a @posix_spawnp@ ENOENT).
  | DiscoveryFailure  !DiscoveryError
  | PlanFailure       !ProjectRoot !PlanError
    -- | @hypha lookup@: @--offline@ suppressed the remote tier.
  | HoogleOffline      !HoogleQuery ![Tier]
    -- | @hypha lookup@: no providers found across every tier consulted.
  | HoogleNotFound     !HoogleQuery ![Tier]
    -- | @hypha lookup@: the remote Hoogle tier failed.
  | HoogleRemoteError  !HoogleQuery ![Tier] !RemoteError
  deriving stock (Show)

errorCode :: HyphaError -> Text
errorCode = \case
  UserError{}        -> "USER_ERROR"
  NotFound{}         -> "NOT_FOUND"
  NetworkError{}     -> "NETWORK_ERROR"
  ToolMissing{}      -> "TOOL_MISSING"
  DiscoveryFailure{} -> "ENV_ERROR"
  PlanFailure _ e    -> case e of
    PlanNotFound{}     -> "ENV_ERROR"
    PlanParseFailure{} -> "CORRUPTION"
  HackageFailure _ e -> hackageErrorWireCode e
  HoogleOffline{}     -> "HOOGLE_OFFLINE"
  HoogleNotFound{}    -> "NOT_FOUND"
  HoogleRemoteError{} -> "HOOGLE_REMOTE_ERROR"

-- | Wire-code dispatch for the variants of 'HackageError'.  Kept here
-- rather than next to 'HackageError' itself so the mapping stays in
-- sync with 'hackageErrorExitCode' below — they MUST agree.
hackageErrorWireCode :: HackageError -> Text
hackageErrorWireCode = \case
  Hackage.NetworkError{}     -> "NETWORK_ERROR"
  Hackage.HttpError{}        -> "NETWORK_ERROR"
  Hackage.OfflineCacheMiss{} -> "NOT_FOUND"
  Hackage.DecodeError{}      -> "CORRUPTION"
  Hackage.MissingField{}     -> "CORRUPTION"

hackageErrorExitCode :: HackageError -> ExitCode
hackageErrorExitCode = \case
  Hackage.NetworkError{}     -> exitNetworkError
  Hackage.HttpError{}        -> exitNetworkError
  Hackage.OfflineCacheMiss{} -> exitNotFound
  Hackage.DecodeError{}      -> exitCacheError
  Hackage.MissingField{}     -> exitCacheError

errorMessage :: HyphaError -> Text
errorMessage = \case
  UserError         reason -> renderUserErrorReason reason
  NotFound          reason -> renderNotFoundReason  reason
  HackageFailure    name e -> renderHackageError    name e
  NetworkError      se     -> Text.pack (displayException se)
  ToolMissing       t  ioe ->
    renderTool t <> " not available: " <> Text.pack (displayException ioe)
  DiscoveryFailure  (NoProjectFound location)
    -> "no cabal project found (searched up from " <> Text.pack location <> ")"
  PlanFailure (ProjectRoot r) e -> case e of
    PlanNotFound pth ->
         "plan.json missing under " <> Text.pack r
      <> " (cabal-plan: " <> Text.pack pth <> ")"
      <> "; run `cabal build --dry-run`"
    PlanParseFailure m -> "plan.json parse failure: " <> Text.pack m
  HoogleOffline      _ _ ->
    "--offline (or HYPHA_OFFLINE) suppresses remote tier"
  HoogleNotFound     _ _ ->
    "no providers found"
  HoogleRemoteError  _ _ remoteErr -> renderRemoteError remoteErr

errorExitCode :: HyphaError -> ExitCode
errorExitCode = \case
  UserError{}        -> exitUserError
  NotFound{}         -> exitNotFound
  NetworkError{}     -> exitNetworkError
  ToolMissing{}      -> exitToolMissing
  DiscoveryFailure{} -> exitEnvironmentError
  PlanFailure _ e    -> case e of
    PlanNotFound{}     -> exitEnvironmentError
    PlanParseFailure{} -> exitCacheError
  HackageFailure _ e  -> hackageErrorExitCode e
  HoogleOffline{}     -> exitNetworkError
  HoogleNotFound{}    -> exitNotFound
  HoogleRemoteError{} -> exitCacheError

-- | Envelope-level @actions@ map derived from the error constructor.
-- Most errors carry no command-specific recovery hints; the lookup
-- variants do, and the query / tier list / 'RemoteError' embedded in
-- the constructor are rendered here — never at the call site.
errorActions :: HyphaError -> Map Text Text
errorActions = \case
  HoogleOffline (HoogleQuery q) tiers -> Map.fromList
    [ ("retry_online",    "hypha lookup " <> q)
    , ("query",           q)
    , ("tiers_consulted", renderTierList tiers)
    ]
  HoogleNotFound (HoogleQuery q) tiers -> Map.fromList
    [ ("retry_with_prefix", "hypha lookup " <> q <> "*")
    , ("query",             q)
    , ("tiers_consulted",   renderTierList tiers)
    ]
  HoogleRemoteError (HoogleQuery q) tiers _remoteErr -> Map.fromList
    [ ("retry_offline",
        "hypha lookup " <> q <> " --offline")
    , ("raise_timeout",
        "HYPHA_HOOGLE_TIMEOUT=30 hypha lookup " <> q)
    , ("query",           q)
    , ("tiers_consulted", renderTierList tiers)
    ]
  _ -> Map.empty

-- | 'ExceptT'-friendly wrapper around 'discoverProjectRoot'.
discoverProjectRootE
  :: MonadIO m
  => Maybe FilePath -> ExceptT HyphaError m ProjectRoot
discoverProjectRootE mDir =
  ExceptT (liftIO (first DiscoveryFailure <$> discoverProjectRoot mDir))

-- | 'ExceptT'-friendly wrapper around 'loadBuildPlan'.  Threads the
-- 'ProjectRoot' into 'PlanFailure' so messages can name the directory.
loadBuildPlanE
  :: MonadIO m
  => ProjectRoot -> ExceptT HyphaError m BuildPlan
loadBuildPlanE root =
  ExceptT (liftIO (first (PlanFailure root) <$> loadBuildPlan root))
