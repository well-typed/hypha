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
    -- * Classification
  , errorCode
  , errorMessage
  , errorExitCode
  , errorActions
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text

import Hypha.Exit
  ( ExitCode, exitUserError, exitNotFound, exitNetworkError, exitCacheError
  , exitEnvironmentError )
import qualified Hypha.Cabal.RepoCache as RepoCache
import Hypha.Hackage.Api (HackageError, renderHackageError)
import qualified Hypha.Hackage.Api as Hackage
import Hypha.Hoogle.Remote (RemoteError, renderRemoteError)
import Hypha.Hoogle.Tier (Tier, renderTierList)
import Hypha.Hoogle.Type (HoogleQuery (..))
import Hypha.Project.Discovery (DiscoveryError (..))
import Hypha.Project.Overrides (OverrideError, renderOverrideError)
import Hypha.Project.Plan (PlanError (..))
import Hypha.Server.Bind (BindError, renderBindError)
import Hypha.Source.Reach
  ( ReachGap, SymbolSearchFailure (..), renderReachGap
  , renderSymbolSearchFailure )
import Hypha.Types.BuildPlan (ProjectRoot (..))
import Hypha.Types.PackageId
  ( PackageId (..), PackageName (..), renderPackageId )
import Hypha.Types.SymbolPath (ModulePath (..), SymbolName (..))

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
    --
    -- Carries /why/, because \"not found\" is four different facts and
    -- only one of them means the symbol is absent: the search may have
    -- stopped at a hop limit or a parse budget, and it may have been
    -- looking through a dependency graph with holes in it.  A consumer
    -- that cannot tell those apart cannot tell a retry-worthy answer from
    -- a settled one.
  | NotFoundSymbol
      !PackageId
      !ModulePath
      !SymbolName
      !SymbolSearchFailure
      ![ReachGap]
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
  NotFoundSymbol           pid asking sym failure gaps ->
    renderPid pid <> ": "
      <> renderSymbolSearchFailure asking sym failure
      <> renderGaps gaps

-- | The dependencies a search could not read, appended to the reason it
-- came up short.
--
-- Part of the message rather than a footnote: a symbol that is genuinely
-- absent and a symbol whose defining package was never unpacked produce
-- the same \"not found\" otherwise, and the second is fixed by
-- @cabal build@ while the first is not.
renderGaps :: [ReachGap] -> Text
renderGaps []   = ""
renderGaps gaps =
  " (also: " <> Text.intercalate "; " (map renderReachGap gaps) <> ")"

renderPid :: PackageId -> Text
renderPid = renderPackageId

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
-- 'HackageError' — never pre-rendered 'Text'.  Rendering happens in
-- 'errorMessage' / 'errorActions', at the wire boundary.
--
-- There is no catch-all umbrella constructor for "any exception at
-- all".  Library code only catches the specific exception types it
-- knows how to handle structurally (e.g. 'HttpException' inside
-- 'Hypha.Hoogle.Remote' and 'Hypha.Hackage.*' for transport failures);
-- everything else propagates and is rendered as a single
-- @INTERNAL_ERROR@ envelope by the top-level @catchAny@ in
-- @app/hypha/Main.hs@.
data HyphaError
  = UserError         !UserErrorReason
  | NotFound          !NotFoundReason
  | HackageFailure    !PackageName !HackageError
    -- ^ Structured Hackage cause.  Dispatch on the variant for wire
    --   code / exit code mapping.
  | DiscoveryFailure  !DiscoveryError
  | PlanFailure       !ProjectRoot !PlanError
    -- | @hypha lookup@: @--offline@ suppressed the remote tier.
  | HoogleOffline      !HoogleQuery ![Tier]
    -- | @hypha lookup@: no providers found across every tier consulted.
  | HoogleNotFound     !HoogleQuery ![Tier]
    -- | @hypha lookup@: the remote Hoogle tier failed.
  | HoogleRemoteError  !HoogleQuery ![Tier] !RemoteError
  deriving stock (Show, Eq)

errorCode :: HyphaError -> Text
errorCode = \case
  UserError{}        -> "USER_ERROR"
  NotFound{}         -> "NOT_FOUND"
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
  Hackage.TarballFailure{}   -> "CORRUPTION"

hackageErrorExitCode :: HackageError -> ExitCode
hackageErrorExitCode = \case
  Hackage.NetworkError{}     -> exitNetworkError
  Hackage.HttpError{}        -> exitNetworkError
  Hackage.OfflineCacheMiss{} -> exitNotFound
  Hackage.DecodeError{}      -> exitCacheError
  Hackage.MissingField{}     -> exitCacheError
  Hackage.TarballFailure{}   -> exitCacheError

errorMessage :: HyphaError -> Text
errorMessage = \case
  UserError         reason -> renderUserErrorReason reason
  NotFound          reason -> renderNotFoundReason  reason
  HackageFailure    name e -> renderHackageError    name e
  DiscoveryFailure  (NoProjectFound location)
    -> "no cabal project found (searched up from " <> Text.pack location <> ")"
  PlanFailure (ProjectRoot r) e -> case e of
    PlanNotFound pth ->
         "plan.json missing under " <> Text.pack r
      <> " (cabal-plan: " <> Text.pack pth <> ")"
      <> "; run `cabal build --dry-run`"
    PlanParseFailure m -> "plan.json parse failure: " <> Text.pack m
  HoogleOffline      _ _ ->
    "--offline suppresses remote tier"
  HoogleNotFound     _ _ ->
    "no providers found"
  HoogleRemoteError  _ _ remoteErr -> renderRemoteError remoteErr

errorExitCode :: HyphaError -> ExitCode
errorExitCode = \case
  UserError{}        -> exitUserError
  NotFound{}         -> exitNotFound
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
  -- The one 'NotFound' that carries structure worth exposing: an agent
  -- deciding whether to retry, widen, or believe the answer needs to know
  -- whether the search finished, and if it stopped, on which bound and at
  -- which modules.  Rendered here, at the wire boundary, from the values
  -- the search already had.
  NotFound (NotFoundSymbol pid asking sym failure gaps) -> Map.fromList $
    [ ("package", renderPackageId pid)
    , ("module",  unModulePath asking)
    , ("symbol",  unSymbolName sym)
    , ("search",  searchOutcome gaps failure)
    ]
      <> searchDetail failure
      <> [ ("unreadable_dependencies",
              Text.intercalate "; " (map renderReachGap gaps))
         | not (null gaps)
         ]
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
        "hypha lookup " <> q <> " --hoogle-timeout 30")
    , ("query",           q)
    , ("tiers_consulted", renderTierList tiers)
    ]
  HackageFailure _ (Hackage.TarballFailure tErr) -> tarballRecoveryActions tErr
  _ -> Map.empty

-- | Whether the search settled the question or stopped short of it.
--
-- A few words rather than a sentence, so a consumer can branch on the only
-- distinction that changes what it should do next; 'searchDetail' carries
-- the specifics.
--
-- The gaps are an input because a drained frontier means two different
-- things depending on them.  Every candidate read and none declaring the
-- name is a settled question: @exhausted@, believe it.  A frontier that
-- drained while something in it could not be read at all settles nothing —
-- and reporting that as @exhausted@ told the reader the search had
-- established an absence it never looked at.  That is the same lie
-- 'SymbolSearchFailure' was introduced to stop telling about bounds, in
-- the one field an agent actually branches on.
searchOutcome :: [ReachGap] -> SymbolSearchFailure -> Text
searchOutcome gaps = \case
  SearchHopLimit{}     -> "stopped_at_bound"
  SearchParseBudget{}  -> "stopped_at_bound"
  -- Established from the asking module's own parse, so nothing outside it
  -- could have changed the answer: a gap is not a reason to doubt these.
  SearchNotExported{}  -> "exhausted"
  SearchNotDeclared{}  -> "exhausted"
  -- The chain left the component, so what could not be read bears
  -- directly on whether the answer means anything.
  SearchNoSupplier{}   -> blockedIfAnyGaps
  SearchModuleUnparsed{} -> blockedIfAnyGaps
  SearchSweptPackage{} -> blockedIfAnyGaps
  -- Neither "we stopped looking" nor "we looked and it is not there": the
  -- module the question named is not in this package, so no search over
  -- its symbols was ever meaningful.  A consumer should fix the module
  -- name, not widen a bound or believe an absence.
  SearchModuleAbsent   -> "module_absent"
  where
    -- @unreadable_dependencies@ already carries which ones, so the verdict
    -- only has to say that the outcome rests on them.
    blockedIfAnyGaps
      | null gaps = "exhausted"
      | otherwise = "blocked"

-- | The values the search stopped on, for the cases that have any.
searchDetail :: SymbolSearchFailure -> [(Text, Text)]
searchDetail = \case
  SearchNoSupplier cands ->
    [ ("candidates_considered", renderModuleList cands) ]
  SearchHopLimit limit frontier ->
    [ ("hop_limit",             Text.pack (show limit))
    , ("stopped_at",            renderModuleList frontier)
    ]
  SearchParseBudget budget ->
    [ ("parse_budget",          Text.pack (show budget)) ]
  SearchNotDeclared m    -> [ ("resolved_to", unModulePath m) ]
  SearchModuleUnparsed m -> [ ("resolved_to", unModulePath m) ]
  SearchSweptPackage dir -> [ ("scanned",     Text.pack dir) ]
  SearchNotExported      -> []
  SearchModuleAbsent     -> []
  where
    renderModuleList = Text.intercalate ", " . map unModulePath

-- | Recovery hints for 'Hackage.TarballFailure'.  The on-disk path is
-- already in the structured 'TarballError'; surface it to the agent
-- alongside a refresh suggestion so the user does not have to dig the
-- path out of the rendered message.
tarballRecoveryActions :: RepoCache.TarballError -> Map Text Text
tarballRecoveryActions = \case
  RepoCache.TarballMissing       p -> bundle p
  RepoCache.TarballReadError     p _ -> bundle p
  RepoCache.TarballExtractError  p _ -> bundle p
  RepoCache.TarballLayoutError   p _ -> bundle p
  where
    bundle p = Map.fromList
      [ ("tarball_path", Text.pack p)
      , ("delete_tarball", Text.pack ("rm " <> p))
      , ("refresh_index", "cabal update")
      ]
