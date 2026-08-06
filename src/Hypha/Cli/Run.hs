{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Hypha.Cli.Run
  ( -- * Command lifecycles
    runClientMain
  , runServerMain
    -- * Last-resort crash rendering (shared with the hypha-mcp binary)
  , reportInternalError
    -- * Internals exposed for testing
  , processInternalError
  , runClientCommand
  ) where

import Control.Exception (IOException, displayException, fromException, ErrorCall (..))
import Control.Exception.Safe (SomeException (..), bracket, try, tryAny)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.Aeson qualified as Aeson
import Data.Aeson (Value)
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as LBS
import qualified Data.Aeson.Yaml as Aeson.Yaml
import Data.Map.Strict qualified as Map
import Data.Maybe (maybeToList)
import Data.Set qualified as Set
import Data.Set (Set)
import Data.Text.Encoding qualified as Text
import Data.Text qualified as Text
import Data.Text (Text)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import Hypha.BuildEnv.Cabal (CabalStoreError (..), mkCabalBuildEnv)
import Hypha.BuildEnv.Type (BuildEnv (..))
import Hypha.Cache (hackageCacheDir)
import Hypha.Cache qualified as Cache
import Hypha.Cli.Parser
import Hypha.Command.Deps qualified as Deps
import Hypha.Command.Doctor qualified as Doctor
import Hypha.Command.Lookup qualified as Lookup
import Hypha.Command.Module qualified as Module
import Hypha.Command.Package qualified as Package
import Hypha.Command.Server qualified as Server
import Hypha.Command.Source qualified as Source
import Hypha.Command.Symbol qualified as Symbol
import Hypha.Command.Versions qualified as Versions
import Hypha.Error
import Hypha.Exit (exitInternalError, toSystemExitCode)
import Hypha.Hackage.Api (HackageClient, mkHackageClient, mkOfflineHackageClient)
import Hypha.Hoogle.Local qualified as HogLocal
import Hypha.Hoogle.Remote qualified as HogRemote
import Hypha.Hoogle.Type (HoogleQuery (..))
import Hypha.Logging (LogEvent (..))
import Hypha.Output.Json
import Hypha.Output.Outcome
import Hypha.Package.Resolver
import Hypha.Prelude (warnOnLeft)
import Hypha.Project.Components qualified as Comp
import Hypha.Project.Discovery (discoverProjectRoot)
import Hypha.Project.Fingerprint qualified as Fingerprint
import Hypha.Project.Overrides (parsePackageOverride)
import Hypha.Project.Plan (loadBuildPlan, planHash)
import Hypha.Search.PackageCache qualified as PC
import Hypha.Source.Dependencies (dependencyReach)
import Hypha.Source.Origins qualified as Origins
import Hypha.Types
import Hypha.Types.BuildPlan
import Hypha.Types.PackageId
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Directory
import System.Exit qualified as System
import System.FilePath ((</>), takeDirectory, takeFileName)
import System.IO (IOMode (..), hClose, hFlush, hPutStrLn, stderr, stdout, withFile)
import System.Process (readProcessWithExitCode)

traceStart :: Monad m => HyphaM m ()
traceStart = trace (LogInfo "starting hypha")

-- | Client-command lifecycle: run the command, emit exactly one
-- envelope on stdout (YAML by default, JSON with @--json@), exit with
-- the typed code.
runClientMain :: HyphaOptions -> ClientCommand -> IO ()
runClientMain opts ccmd = do
  result <- tryAny (runHypha opts (traceStart *> runClientCommand ccmd))
  case result of
    Left exc          -> processInternalError opts tag exc
    Right (Left err)  -> processError opts tag err
    Right (Right oc)  -> processOutcome opts oc
  where
    tag = ClientTag (clientCommandTag ccmd)

-- | Server lifecycle: no 'Outcome', no success envelope — the server
-- blocks inside Warp and a clean return is a clean shutdown.  Errors
-- still get the structured treatment: reified failures ('HyphaError',
-- e.g. a malformed @--bind@) render an error envelope tagged
-- @\"server\"@, and escaped exceptions render @INTERNAL_ERROR@.
runServerMain :: HyphaOptions -> ServerCommand -> IO ()
runServerMain opts (ServerCommand port mBind prebuild jobs) = do
  result <- tryAny . runHypha opts $
    traceStart *> runServerInteractive port mBind prebuild jobs
  case result of
    Left exc         -> processInternalError opts ServerTag exc
    Right (Left err) -> processError opts ServerTag err
    Right (Right ()) -> pure ()

-- | Load the project root + build plan (with overrides applied).
-- Used by command arms that need a typed plan but do not need full
-- resolver/build-env machinery.
loadPlan :: Hypha (ProjectRoot, BuildPlan)
loadPlan = do
  opts      <- askOpts
  cacheRoot <- asks heCacheDir
  root      <- mapEitherIO DiscoveryFailure (discoverProjectRoot (hoProjectDir opts))
  rawPlan   <- mapEitherIO (PlanFailure root) (loadBuildPlan cacheRoot root)
  overrides <- collectOverrides (hoPackageOverrides opts)
  pure (root, applyOverrides overrides rawPlan)

-- | Parse the @--package-override@ list, throwing 'UserError' on
-- malformed values.
collectOverrides
  :: MonadError HyphaError m => [Text] -> m [PackageOverride]
collectOverrides raws = case traverse parsePackageOverride raws of
  Left  err -> throwError (UserError (UserOverrideParse err))
  Right xs  -> pure xs

-- | Create a Hackage client respecting the offline flag.
mkHackageClientForOpts :: Hypha (HackageClient IO)
mkHackageClientForOpts = do
  opts  <- askOpts
  cacheRoot <- asks heCacheDir
  let hackCache = hackageCacheDir cacheRoot
  liftIO $ if hoOffline opts
    then mkOfflineHackageClient hackCache
    else do
      mgr <- newManager tlsManagerSettings
      mkHackageClient mgr hackCache

-- | Best-effort project + plan loader.  Each failure is announced on
-- @stderr@ before the degraded fallback kicks in — no @Left _ -> ...@
-- silent ignore.  Returns 'Nothing' for the project root when
-- discovery failed, and 'emptyBuildPlan' when plan loading failed.
loadProjectAndPlan :: Hypha (Maybe ProjectRoot, BuildPlan)
loadProjectAndPlan = do
  opts      <- askOpts
  cacheRoot <- asks heCacheDir
  mRoot <- liftIO $ warnOnLeft
             (errorMessage . DiscoveryFailure)
             Nothing
             (fmap Just <$> discoverProjectRoot (hoProjectDir opts))
  plan <- liftIO $ maybe (pure emptyBuildPlan) (loadPlanOrWarn cacheRoot) mRoot
  pure (mRoot, plan)
  where
    loadPlanOrWarn cacheRoot root =
      warnOnLeft (errorMessage . PlanFailure root) emptyBuildPlan
                 (loadBuildPlan cacheRoot root)

-- | Pick the right build-env constructor for the loaded project.
-- Project-less calls degrade to a store-only env (still announces the
-- underlying failure inside 'mkBuildEnv' / 'mkBasicBuildEnv').
mkBuildEnvFor :: Maybe ProjectRoot -> BuildPlan -> IO (BuildEnv IO)
mkBuildEnvFor Nothing     _    = mkBasicBuildEnv
mkBuildEnvFor (Just root) plan = mkBuildEnv root plan

-- | When the build plan is empty (no project / @plan.json@), synthesise
-- one from the packages the 'BuildEnv' reports as installed in the
-- cabal store.  Each synthetic unit carries only the 'PackageId' — no
-- deps, no source directory, no plan-derived metadata — but the
-- @hypha server@ UI and the resolver chain can still browse and look
-- up those packages.  Returns the input plan unchanged when it
-- already has units.
enrichPlanFromStore :: BuildEnv IO -> BuildPlan -> IO BuildPlan
enrichPlanFromStore env plan
  | not (Map.null (bpUnits plan)) = pure plan
  | otherwise = do
      installed <- discoverInstalledPackages env
      ghcVer    <- ghcVersion env
      let units = Map.fromList
            [ (pkgName pid, syntheticUnit pid) | pid <- Set.toList installed ]
      hPutStrLn stderr $
        "note: no cabal plan in scope; browsing "
        <> show (Map.size units)
        <> " packages from the cabal store"
      pure plan
        { bpCompiler = CompilerId ("ghc-" <> unVersion ghcVer)
        , bpUnits    = units
        }
  where
    syntheticUnit pid = PlannedUnit
      { puId            = pid
      , puDeps          = []
      , puIsLocal       = False
      , puOrigin        = OriginHackage
      , puSrcDir        = Nothing
      , puDistDir       = Nothing
      , puLibComponents = []
      }

-- | Build a resolver and associated build-env.  Degrades gracefully
-- when no project / plan is reachable, but every degradation is
-- announced via 'warnOnLeft' so the user is never left guessing why
-- the answer looks empty.  The only hard-fail branch is malformed
-- @--package-override@ values, surfaced as 'UserError'.
loadResolver :: Hypha (PackageResolver IO, BuildEnv IO)
loadResolver = do
  (resolver, env, _plan) <- loadResolverAndPlan
  pure (resolver, env)

-- | 'loadResolver', keeping the plan it was built from.
--
-- The plan is what tells @source@ which packages a re-export may leave
-- into, so a caller that has to follow one needs the same plan the
-- resolver was configured with — a second 'loadProjectAndPlan' would
-- re-read @plan.json@ and could disagree about the overrides.
loadResolverAndPlan :: Hypha (PackageResolver IO, BuildEnv IO, BuildPlan)
loadResolverAndPlan = do
  opts      <- askOpts
  cacheRoot <- asks heCacheDir
  hclient   <- mkHackageClientForOpts
  (mRoot, raw) <- loadProjectAndPlan
  overrides <- collectOverrides (hoPackageOverrides opts)
  let plan = applyOverrides overrides raw
  liftIO $ do
    env      <- mkBuildEnvFor mRoot plan
    resolver <- mkPackageResolver env hclient cacheRoot plan
    pure (resolver, env, plan)

-- | Create a basic BuildEnv (store only, no project source dirs).
-- The active GHC's version is sniffed from @PATH@ via
-- @ghc --numeric-version@; if that fails, we fall back to enumerating
-- the cabal store and picking the first @ghc-*@ directory we find.
-- After every probe has failed, the aggregated reasons are reported
-- to @stderr@ so the user understands why the env collapsed to
-- 'offlineNullBuildEnv' rather than a real store-backed one.
mkBasicBuildEnv :: IO (BuildEnv IO)
mkBasicBuildEnv = do
  home <- getHomeDirectory
  let storeBase = home </> ".cabal" </> "store"
  candidates <- candidateStoreDirs storeBase
  tryStores candidates []
  where
    tryStores [] errs = do
      hPutStrLn stderr $
        "warning: no usable cabal store found; falling back to null BuildEnv"
        <> concatMap (\(p, e) -> "\n  - " <> p <> ": "
                                 <> Text.unpack (renderCabalStoreError e))
                     (reverse errs)
      ghcVer <- sniffGhcOrUnknown
      pure (offlineNullBuildEnv ghcVer)
    tryStores (p:ps) errs = do
      eEnv <- mkCabalBuildEnv p
      case eEnv of
        Right env -> pure env
        Left  err -> tryStores ps ((p, err) : errs)

-- | Best-effort list of @ghc-*@ store directories to probe.  Prefers
-- the version reported by @ghc --numeric-version@ on @PATH@; if that
-- is unavailable, enumerates whatever @ghc-*@ entries the store
-- already has.
candidateStoreDirs :: FilePath -> IO [FilePath]
candidateStoreDirs storeBase = do
  mFromPath <- detectGhcVersionFromPath
  enumerated <- enumerateStoreGhcDirs storeBase
  let preferred = maybeToList (fmap (\v -> "ghc-" <> Text.unpack v) mFromPath)
      ordered   = preferred <> [ d | d <- enumerated, d `notElem` preferred ]
  pure (map (storeBase </>) ordered)

-- | Enumerate @ghc-*@ subdirectories of the cabal store, if any.
enumerateStoreGhcDirs :: FilePath -> IO [FilePath]
enumerateStoreGhcDirs storeBase = do
  ok <- doesDirectoryExist storeBase
  if not ok
    then pure []
    else do
      entries <- listDirectory storeBase
      pure [ e | e <- entries, "ghc-" `Text.isPrefixOf` Text.pack e ]

-- | Sniff the active GHC's version by running @ghc --numeric-version@
-- on @PATH@.  Returns 'Nothing' if @ghc@ is not on @PATH@ or returns
-- an unexpected exit code.  Only 'IOException' is caught — that is
-- what a missing binary raises; anything else is a genuine crash and
-- must reach the internal-error path.
detectGhcVersionFromPath :: IO (Maybe Text)
detectGhcVersionFromPath = do
  r <- try @IO @IOException
         (readProcessWithExitCode "ghc" ["--numeric-version"] "")
  case r of
    Right (System.ExitSuccess, out, _) ->
      let v = Text.strip (Text.pack out)
      in pure (if Text.null v then Nothing else Just v)
    _ -> pure Nothing

renderCabalStoreError :: CabalStoreError -> Text
renderCabalStoreError = \case
  StoreNotFound p   -> "store directory missing (" <> Text.pack p <> ")"
  GhcVersionUnknown -> "could not determine GHC version from store path"

-- | Per-command dispatch.
runClientCommand :: ClientCommand -> Hypha (Outcome Value)
runClientCommand = \case
  LookupCommand q ->
    runLookupCommand q

  PackageCommand rawArg -> do
    let ref = parsePackageRef rawArg
    (resolver, env) <- loadResolver
    rp       <- liftEitherIO (resolveRef resolver ref)
    modules0 <- liftIO (resolveExposedModules resolver env (rpPkgId rp))
    pure $ Package.mkSuccessOutcome
      (unPackageName (refName ref))
      (pkgVersion (rpPkgId rp))
      (rpIsLocal rp)
      (rpDepsCount rp)
      (rpOrigin rp)
      modules0

  VersionsCommand rawArg -> do
    let PackageRef pkgName _ = parsePackageRef rawArg
    (resolver, _env) <- loadResolver
    eAvail    <- liftIO (fetchVrs resolver pkgName)
    (_, plan) <- loadPlan
    case eAvail of
      Left err -> do
        -- Hackage availability is best-effort; surface the cause on
        -- stderr (per the "never ignore an error branch silently"
        -- ethos in CLAUDE.md) and fall back to the plan-only view.
        liftIO $ hPutStrLn stderr $
          "warning: hackage availability lookup failed: "
          <> Text.unpack (errorMessage err)
        pure (Versions.runVersionsWithAvail plan pkgName [])
      Right versions ->
        pure (Versions.runVersionsWithAvail plan pkgName versions)

  ModuleCommand arg -> do
    (pkgT, modPath)  <- parsePkgMod arg
    (resolver, _env) <- loadResolver
    rp <- liftEitherIO (resolveRef resolver (parsePackageRef pkgT))
    let pid = rpPkgId rp
    d  <- liftEitherIO (resolveSrc resolver pid)
    oc <- liftIO (Module.runModuleFromDir d pid modPath)
    pure (tagOutsidePlan oc (rpIsOutsidePlan rp))

  SymbolCommand arg -> runSymbolArm arg

  SourceCommand arg -> do
    (pkgT, modPath, mSym) <- parsePkgModOptSym arg
    runSourceArm (parsePackageRef pkgT) modPath mSym

  DepsCommand rawArg reverseMode mDepth -> do
    let PackageRef pkgName _ = parsePackageRef rawArg
    (_, plan) <- loadPlan
    liftIO (Deps.runDeps plan pkgName reverseMode mDepth)

  DoctorCommand ->
    liftIO Doctor.runDoctor

-- | Parse @PKG/MOD@, throwing 'UserError' on malformed input.
parsePkgMod :: MonadError HyphaError m => Text -> m (Text, Text)
parsePkgMod arg = case Text.splitOn "/" arg of
  [pkg, modPath] -> pure (pkg, modPath)
  _              -> throwError (UserError (UserExpectedPkgMod arg))

-- | Parse @PKG/MOD[/SYM]@, throwing 'UserError' on malformed input.
parsePkgModOptSym
  :: MonadError HyphaError m => Text -> m (Text, Text, Maybe Text)
parsePkgModOptSym arg = case Text.splitOn "/" arg of
  [pkg, modPath]      -> pure (pkg, modPath, Nothing)
  [pkg, modPath, sym] -> pure (pkg, modPath, Just sym)
  _                   -> throwError (UserError (UserExpectedPkgModOptSym arg))

-- | Server interactive arm.  Refuses non-loopback binds with exit 2; on
-- successful bind it blocks inside Warp until interrupted.  Returning
-- @()@ means Warp shut down cleanly — 'runServerMain' turns that into
-- a normal process exit, so no @System.exit*@ is ever raised inside
-- the guarded region.
runServerInteractive :: Int -> Maybe Text -> Bool -> Int -> Hypha ()
runServerInteractive port mBind prebuild jobs = do
  cacheRoot       <- asks heCacheDir
  ba              <- bindAddrFromFlags port mBind
  (mRoot, plan0)  <- loadProjectAndPlan
  hclient         <- mkHackageClientForOpts
  env             <- liftIO (mkBuildEnvFor mRoot plan0)
  plan            <- liftIO (enrichPlanFromStore env plan0)
  resolver        <- liftIO (mkPackageResolver env hclient cacheRoot plan)
  let serverOpts = Server.ServerOpts ba prebuild (fromIntegral (max 1 jobs)) cacheRoot
  mapEitherIO (UserError . UserBindError)
    (Server.runServer mRoot plan env resolver serverOpts)

-- | Resolve the @--bind@ / @--port@ flags.  Malformed binds map to
-- 'UserError' so the exit code (2) matches user-input failures.
bindAddrFromFlags
  :: MonadError HyphaError m
  => Int -> Maybe Text -> m Server.BindAddr
bindAddrFromFlags port mBind =
  either (throwError . UserError . UserBindError) pure $ case mBind of
    Just raw -> Server.parseBind raw
    Nothing  -> case Server.portFromInt port of
      Just pn -> Right (Server.defaultBindAddr pn)
      Nothing -> Left (Server.BindMalformed
        (Text.pack ("--port out of range: " <> show port)))

-- | Source command arm: resolve package (plan → store → Hackage), locate
-- source directory (local → Hackage tarball), then extract snippet.
runSourceArm
  :: PackageRef -> Text -> Maybe Text -> Hypha (Outcome Value)
runSourceArm ref modPath mSym = do
  (resolver, _env, plan) <- loadResolverAndPlan
  rp  <- liftEitherIO (resolveRef resolver ref)
  let pid = rpPkgId rp
  dir <- liftEitherIO (resolveSrc resolver pid)
  -- The plan is the dependency graph a cross-package re-export is
  -- followed through.  Outside a project it is empty, which makes the
  -- reach empty too: the plan-less path keeps reporting the re-export it
  -- cannot follow rather than guessing at one.
  reach <- liftIO (dependencyReach plan resolver (ownerOracleFor plan) pid)
  oc  <- liftEitherIO (Source.runSourceFromDir reach pid dir modPath mSym)
  pure (tagOutsidePlan oc (rpIsOutsidePlan rp))

-- | Symbol command arm.  Shares the reach with 'runSourceArm', because it
-- shares the question: the module a user names is often a facade, and a
-- card that stops at the facade has nothing to say.
runSymbolArm :: Text -> Hypha (Outcome Value)
runSymbolArm arg = do
  (resolver, env, plan) <- loadResolverAndPlan
  -- The producer rather than a reach: a reach is anchored on the package
  -- being asked about, and the argument naming it is parsed inside
  -- 'Symbol.runSymbolWith'.  Handing over 'dependencyReach' partially
  -- applied keeps that parse in one place.
  liftEitherIO
    (Symbol.runSymbolWith env resolver
       (dependencyReach plan resolver (ownerOracleFor plan)) arg)

-- | How the reach asks which unit owns a module: @ghc-pkg@ over the global
-- database and every store database we can find.
--
-- Returned as the action itself, unrun.  It selects the plan's compiler by
-- executing it, and a query the unpacked dependencies already answer must
-- not pay for that — nor should a machine whose @ghc@ does not match the
-- plan hear about it on a query that succeeded.  A database root we cannot
-- list is reported rather than dropped: every unit under it would
-- otherwise look like one nothing owns.
ownerOracleFor
  :: BuildPlan -> IO (Either Origins.OriginError (Origins.ModuleOwnerOracle IO))
ownerOracleFor plan = do
  (dbs, unreadable) <- Origins.discoverPackageDbs compiler
  mapM_ (warnText . Origins.renderOriginError) unreadable
  Origins.mkGhcModuleOwnerOracle compiler dbs
  where
    compiler = bpCompiler plan
    warnText t = hPutStrLn stderr ("hypha: " <> Text.unpack t)

-- | Drive the tiered @lookup@ command.  Builds the package cache
-- and project Hoogle handle, then runs the cascade.  Project root
-- discovery is best-effort: outside a cabal project, only the global
-- cache and remote Hoogle are consulted.
runLookupCommand :: Text -> Hypha (Outcome Value)
runLookupCommand q = do
  -- No catch-all 'try' around this block: HTTP failures are caught
  -- (and converted to 'RemoteError') inside Hoogle.Remote, every other
  -- structurally-handled failure flows through 'HyphaError' explicitly,
  -- and genuinely-unexpected exceptions bubble up to the top-level
  -- 'catchAny' in @app/hypha/Main.hs@ where they become a single
  -- structured @INTERNAL_ERROR@ envelope.
  opts <- askOpts
  cacheRoot <- asks heCacheDir
  mRoot <- liftIO $ warnOnLeft
             (errorMessage . DiscoveryFailure)
             Nothing
             (fmap Just <$> discoverProjectRoot (hoProjectDir opts))
  cache <- liftIO $ PC.openPackageCache mRoot
  dotHypha <- liftIO $ case mRoot of
    Just (ProjectRoot r) -> do
      let d = r </> ".hypha"
      createDirectoryIfMissing True d
      pure d
    Nothing -> do
      x <- Cache.cacheRoot
      let d = x </> "no-project"
      createDirectoryIfMissing True d
      pure d
  storeRoot <- liftIO defaultStoreRoot
  distRoot  <- liftIO defaultDistDocRoot
  hoogleLocal <- liftIO $ HogLocal.openLocalHoogle dotHypha storeRoot distRoot

  -- Tier 2 prep is deferred: 'runLookup' invokes 'loPrepareLocal' only
  -- when the package-cache tier misses.  Cache hits are typically
  -- sub-millisecond, while ensuring the Hoogle DB freshness can run
  -- the haddock generator over the whole plan (multi-second, noisy).
  -- The verbose flag also gates the underlying 'hoogle' library
  -- chatter; on the silent path we redirect both stdout and stderr
  -- of the indexing step to @/dev/null@ so the JSON envelope stays
  -- the only thing on stdout and the agent's stderr stays clean.
  let prepLocal :: IO ()
      prepLocal = case mRoot of
        Nothing   -> pure ()
        Just root -> withQuietIfNotVerbose (hoVerbose opts) $
          ensureProjectHoogle cacheRoot storeRoot distRoot dotHypha hoogleLocal root

  let lookupOpts = Lookup.LookupOptions
        { Lookup.loOffline      = hoOffline opts
        , Lookup.loRemote       =
            HogRemote.defaultRemoteOptions
              { HogRemote.roOffline = hoOffline opts }
        , Lookup.loPrepareLocal = prepLocal
        }
  liftEitherIO (Lookup.runLookup cache hoogleLocal lookupOpts (HoogleQuery q))

-- | Materialise the project Hoogle DB: load the plan, derive a
-- 'HoogleStamp' (plan hash + aggregate source-tree fingerprint),
-- enumerate the units, and call 'ensureFresh'.  Tier 2 is
-- best-effort: failures do not abort the lookup, but every one is
-- announced on @stderr@ (see the body).
ensureProjectHoogle
  :: FilePath          -- ^ cache root
  -> FilePath          -- ^ store root
  -> FilePath          -- ^ dist doc root
  -> FilePath          -- ^ project @.hypha@ directory
  -> HogLocal.HyphaHoogle
  -> ProjectRoot
  -> IO ()
ensureProjectHoogle cacheRoot storeRoot distRoot dotHypha _ root = do
  -- Best-effort: ANY failure here (missing toolchain in a sandbox,
  -- IO errors regenerating the DB, Hoogle library panics) must not
  -- abort the lookup. The cascade in 'Lookup.runLookup' is designed
  -- to fall through to remote Hoogle when Tier 2 yields no hits, so
  -- we keep going on failure — but every failure is announced on
  -- stderr so the user is never left wondering why Tier 2 went silent.
  r <- try @IO @SomeException $ do
    ePlan <- loadBuildPlan cacheRoot root
    case ePlan of
      Left planErr ->
        hPutStrLn stderr $
          "warning: project Hoogle DB skipped — "
          <> Text.unpack (errorMessage (PlanFailure root planErr))
      Right plan -> do
        let units = planToLocalUnits plan
            ph    = planHash plan
        fp <- aggregateFingerprint units
        let stamp = HogLocal.HoogleStamp ph fp
        HogLocal.ensureFresh HogLocal.defaultHaddockRunner
                             storeRoot distRoot dotHypha stamp units
  case r of
    Left e  -> hPutStrLn stderr $
                 "warning: project Hoogle DB skipped — "
                 <> displayException e
    Right _ -> pure ()

-- | Run an action with @stdout@ and @stderr@ redirected to
-- @\/dev\/null@ — except under @--verbose@, in which case the handles
-- pass through untouched.
--
-- The local-Hoogle indexing path calls 'Hoogle.hoogle' (the upstream
-- library), which writes its own progress lines (@"Starting generate"@,
-- @"[1\/22] array... 0.05s"@, ...) straight to the host's @stdout@ /
-- @stderr@.  Letting those leak through corrupts our @stdout@ contract
-- (the JSON envelope is the only thing the agent should ever see
-- there) and bloats the agent's stderr capture for no token-economic
-- gain.  We flush, duplicate the original fds, splice in @\/dev\/null@,
-- run the action, and restore on exit so a panic inside the action
-- can't leave the process writing to the bit-bucket forever.
withQuietIfNotVerbose :: Bool -> IO a -> IO a
withQuietIfNotVerbose True  io = io
withQuietIfNotVerbose False io = do
  hFlush stdout
  hFlush stderr
  withFile "/dev/null" WriteMode $ \devnull ->
    bracket
      (do savedOut <- hDuplicate stdout
          savedErr <- hDuplicate stderr
          hDuplicateTo devnull stdout
          hDuplicateTo devnull stderr
          pure (savedOut, savedErr))
      (\(savedOut, savedErr) -> do
          hFlush stdout
          hFlush stderr
          hDuplicateTo savedOut stdout
          hDuplicateTo savedErr stderr
          hClose savedOut
          hClose savedErr)
      (const io)

planToLocalUnits :: BuildPlan -> [HogLocal.LocalUnit]
planToLocalUnits plan =
  [ HogLocal.LocalUnit
      { HogLocal.luPkgId   = puId u
      , HogLocal.luSrcDirs = unitSrcDirs u
      , HogLocal.luIsLocal = puIsLocal u
      }
  | u <- Map.elems (bpUnits plan)
  ]
  where
    unitSrcDirs u = case puLibComponents u of
      []   -> maybeToList (puSrcDir u)
      cs   -> concatMap Comp.ciHsSourceDirs cs

-- | Aggregate fingerprint across every /local/ unit's source roots.
-- Non-local units don't contribute because their bytes are immutable.
aggregateFingerprint :: [HogLocal.LocalUnit] -> IO Text
aggregateFingerprint units = do
  -- Per-unit fingerprints in deterministic order, joined into one
  -- payload before a final SHA-256.  Cost: walks the source tree of
  -- each local unit once.  Cheap on small projects, acceptable on
  -- big ones (still under 100ms for ~10k files).
  perUnit <- mapM unitFp localUnits
  let payload = Text.unlines perUnit
  pure (planHashFromText payload)
  where
    localUnits = [ u | u <- units, HogLocal.luIsLocal u ]
    unitFp u   = Fingerprint.componentFingerprint (HogLocal.luSrcDirs u)

planHashFromText :: Text -> Text
planHashFromText t =
  Text.decodeUtf8
    (Base16.encode (SHA256.hash (Text.encodeUtf8 t)))

-- | Best-effort lookup of the active GHC's cabal store.  When the
-- environment is non-standard we return @\"\"@; 'scavengeStoreTxt'
-- treats that as \"no scavenging available\" and falls back to
-- 'defaultHaddockRunner'.
defaultStoreRoot :: IO FilePath
defaultStoreRoot = do
  home <- getHomeDirectory
  let base = home </> ".cabal" </> "store"
  ok <- doesDirectoryExist base
  if not ok
    then pure ""
    else do
      entries <- listDirectory base
      case [ e | e <- entries, "ghc-" `Text.isPrefixOf` Text.pack e ] of
        (e:_) -> pure (base </> e)
        []    -> pure ""

-- | Best-effort lookup of GHC's distribution doc directory — where
-- boot libs (@base@, @containers@, ...) keep their pre-shipped
-- Hoogle @.txt@.  Invokes @ghc --print-libdir@ and walks back to
-- @share/doc/ghc-X.Y.Z/html/libraries@.  Returns @\"\"@ if @ghc@ is
-- not on PATH; scavenging treats that as \"disabled\".
defaultDistDocRoot :: IO FilePath
defaultDistDocRoot = do
  r <- try @IO @IOException (readProcessWithExitCode "ghc"
         ["--print-libdir"] "")
  case r of
    Right (System.ExitSuccess, out, _) -> do
      let libDir   = trim out
          -- libDir = <prefix>/lib/ghc-X.Y.Z/lib
          --       or <prefix>/lib/ghc-X.Y.Z (older layout)
          -- We want <prefix>/share/doc/ghc-X.Y.Z/html/libraries.
          prefix1  = takeDirectory libDir              -- ../ghc-X.Y.Z
          ghcVer1  = takeFileName  prefix1
          prefix   = case ghcVer1 of
            "lib" -> takeDirectory prefix1  -- newer layout drop two levels
            _     -> takeDirectory prefix1
          ghcVer   = takeFileName (takeDirectory libDir)
          -- Cover both layouts with a small probe list.
          candidates =
            [ prefix </> "share" </> "doc" </> ghcVer
                       </> "html" </> "libraries"
            , takeDirectory prefix </> "share" </> "doc"
                       </> ghcVer </> "html" </> "libraries"
            ]
      firstExisting candidates
    _ -> pure ""
  where
    trim = reverse . dropWhile (`elem` ("\n\r \t" :: String))
                    . reverse
                    . dropWhile (`elem` ("\n\r \t" :: String))

    firstExisting []     = pure ""
    firstExisting (p:ps) = do
      ok <- doesDirectoryExist p
      if ok then pure p else firstExisting ps

-- | Construct a 'BuildEnv' from a project root and its build plan.
-- Falls through to 'offlineNullBuildEnv' when the cabal store cannot
-- be opened, announcing the underlying 'CabalStoreError' on @stderr@
-- so the degraded state is never silent.
mkBuildEnv :: ProjectRoot -> BuildPlan -> IO (BuildEnv IO)
mkBuildEnv (ProjectRoot _) plan = do
  home <- getHomeDirectory
  ghcVer <- sniffGhcOrUnknown
  let CompilerId cid = bpCompiler plan
      planVer        = Text.takeWhileEnd (/= '-') cid
      -- The plan may carry a sentinel ("unknown") or be empty when
      -- 'loadProjectAndPlan' has fallen back to 'emptyBuildPlan'.
      -- In those cases the plan can't tell us which store to probe,
      -- so we fall through to the PATH-sniffed version instead of
      -- composing nonsense paths like 'ghc-unknown'.
      verForStore = if Text.null planVer || planVer == "unknown"
                      then unVersion ghcVer
                      else planVer
  if verForStore == "unknown"
    then pure (offlineNullBuildEnv ghcVer)
    else do
      let ghcDir   = "ghc-" <> Text.unpack verForStore
          storeDir = home </> ".cabal" </> "store" </> ghcDir
      warnOnLeft renderCabalStoreError (offlineNullBuildEnv ghcVer)
                 (mkCabalBuildEnv storeDir)

-- | Empty BuildEnv used when no cabal store is reachable.  All
-- operations return 'Nothing' / empty sets.  The GHC version is
-- supplied by the caller — sniffed from @PATH@ via
-- 'sniffGhcOrUnknown' so non-project invocations of
-- @hypha server@ / @hypha doctor@ still report the active
-- compiler rather than a misleading @"unknown"@ sentinel.
offlineNullBuildEnv :: Version -> BuildEnv IO
offlineNullBuildEnv ghcVer = BuildEnv
  { discoverInstalledPackages = pure Set.empty
  , locatePackageSource       = \_ -> pure Nothing
  , locateRepoTarball         = \_ -> pure Nothing
  , locateHaddockHtml         = \_ -> pure Nothing
  , ghcVersion                = pure ghcVer
  }

-- | Best-effort sniff of the active GHC on @PATH@.  Returns the
-- reported version when successful; otherwise emits a one-line
-- warning to @stderr@ and falls back to the @"unknown"@ sentinel.
sniffGhcOrUnknown :: IO Version
sniffGhcOrUnknown = do
  mv <- detectGhcVersionFromPath
  case mv of
    Just v  -> pure (Version v)
    Nothing -> do
      hPutStrLn stderr
        "warning: ghc not on PATH; reporting version 'unknown'"
      pure (Version "unknown")

-- | Resolve the list of exposed modules for a package.
--
--   Tries to find the package source on disk first (via 'locatePackageSource'
--   from the 'BuildEnv'); if that succeeds, parses the @.cabal@ file for the
--   @exposed-modules@ stanza.  If the source is not available locally, falls
--   back to downloading via 'resolveSrc' (which fetches from Hackage).
--   Returns an empty list on any error or when source is truly unavailable.
resolveExposedModules
  :: PackageResolver IO -> BuildEnv IO -> PackageId -> IO [Text.Text]
resolveExposedModules resolver env pid = do
  -- Try local source first (fast, no network).
  mLocal <- locatePackageSource env pid
  case mLocal of
    Just dir -> do
      modules <- Comp.getExposedModules dir
      if null modules then trySrcResolver else pure modules
    Nothing -> trySrcResolver
  where
    trySrcResolver :: IO [Text.Text]
    trySrcResolver = do
      eDir <- resolveSrc resolver pid
      case eDir of
        Left err -> do
          hPutStrLn stderr $
            "warning: exposed-modules list empty for "
            <> Text.unpack (unPackageName (pkgName pid))
            <> " — source resolve failed: "
            <> Text.unpack (errorMessage err)
          pure []
        Right dir -> Comp.getExposedModules dir

-- | Envelope-shaping options derived from the global flags.
envelopeOptsFor :: HyphaOptions -> EnvelopeOpts
envelopeOptsFor opts = EnvelopeOpts
  { eoFull       = hoFull opts
  , eoSelect     = maybe [] parseSelectList (hoSelect opts)
  , eoPrettyJson = hoPrettyJson opts
  }

-- | Write a pre-built envelope 'Value' to stdout — YAML by default,
-- JSON under @--json@ — and flush.  Every terminal envelope (success,
-- error, internal error) funnels through here so the two output modes
-- cannot drift apart.
emitEnvelope :: HyphaOptions -> Aeson.Value -> IO ()
emitEnvelope opts envelope = do
  if hoJson opts
    then LBS.hPut stdout (encodeEnvelopeValue (envelopeOptsFor opts) envelope)
    else LBS.hPut stdout (Aeson.Yaml.encode envelope)
  hFlush stdout

-- | Emit a successful outcome to stdout, honouring all output-shaping
-- options, then exit with success.  Error handling is /not/ this
-- function's job — errors propagate through 'Hypha' via 'throwError'
-- and are handled in 'runClientMain' where 'runHypha' returns 'Left'.
processOutcome :: HyphaOptions -> Outcome Value -> IO ()
processOutcome opts outcome = do
  let cmd     = outcomeTag outcome
      envOpts = envelopeOptsFor opts
      envelope = encodeOutcomeEnvelope envOpts
                   (compactKeysFor cmd) (fullKeysFor cmd) outcome
  warnUnmatchedSelect cmd envOpts outcome
  emitEnvelope opts envelope
  System.exitWith System.ExitSuccess

-- | Report the @--select@ names this command cannot answer.
--
-- Without this the projection just drops them: @--select bogus@, or a
-- field of some other command, leaves @result: {}@ and exit 0, which
-- reads as "this symbol has no fields" rather than "you named a field
-- that does not exist here".  The names that /would/ have worked come
-- along so the fix is in the message.
--
-- Not silenced by @--quiet@: it reports a mistake in the invocation, not
-- progress chatter, and an agent that never sees it repeats the call.
warnUnmatchedSelect :: ClientCommandTag -> EnvelopeOpts -> Outcome Value -> IO ()
warnUnmatchedSelect cmd envOpts outcome =
  case unmatchedSelect envOpts (compactKeysFor cmd) (fullKeysFor cmd) outcome of
    ([], _) -> pure ()
    (missed, available) ->
      hPutStrLn stderr . Text.unpack $
        "warning: --select names no field of '" <> clientCommandName cmd <> "': "
        <> Text.intercalate ", " missed
        <> "; this command answers with "
        <> renderAvailable available
  where
    renderAvailable ks
      | Set.null ks = "no fields at all"
      | otherwise   =
          Text.intercalate ", " (Set.toAscList ks)
          <> if eoFull envOpts then "" else " (more under --full)"

-- | Emit a 'HyphaError' as a JSON error envelope on stdout, a one-line
-- @CODE: message@ on stderr, then exit with the typed code.
processError :: HyphaOptions -> CommandTag -> HyphaError -> IO ()
processError opts _tag err = do
  emitEnvelope opts (encodeErrorEnvelope err)
  reportError err
  System.exitWith (toSystemExitCode (errorExitCode err))

-- | Render a genuine crash (i.e. an exception that escaped the library)
-- for the command identified by 'CommandTag'. Library code only
-- catches exceptions it knows how to handle structurally
-- ('HttpException' inside "Hypha.Hoogle.Remote" / "Hypha.Hackage.*");
-- everything else lands here via the 'tryAny' in the lifecycle
-- wrappers.  We emit a single @INTERNAL_ERROR@ envelope on stdout then
-- exit with the dedicated 'exitInternalError' code so callers can
-- distinguish "hypha itself crashed" from any other failure class.
processInternalError :: HyphaOptions -> CommandTag -> SomeException -> IO ()
processInternalError opts _tag e = do
  emitEnvelope opts
    (encodeInternalErrorEnvelope (envelopeMessage e))
  internalErrorExit

-- | Variant of 'processInternalError' for contexts with no parsed
-- command and no output-shaping flags — the @hypha-mcp@ binary's
-- top-level @catchAny@.  Always emits compact YAML.
reportInternalError :: SomeException -> IO ()
reportInternalError e = do
  LBS.hPut stdout
    (Aeson.Yaml.encode (encodeInternalErrorEnvelope (envelopeMessage e)))
  hFlush stdout
  internalErrorExit

-- | Render an escaped exception for the envelope's @message@ field.
--
-- 'displayException' on the 'SomeException' wrapper appends GHC's
-- @HasCallStack@ backtrace (GHC ≥ 9.10), and 'ErrorCall' itself carries a
-- legacy CallStack in its location string on all GHC versions.
-- Structurally extract just the message: for 'ErrorCall' (what 'error'
-- throws) the 'ErrorCall' pattern synonym discards the location; for
-- everything else, pattern matching on 'SomeException' drops the
-- 'ExceptionContext' (and thus the 'Backtraces' annotation) on GHC ≥ 9.10.
envelopeMessage :: SomeException -> Text
envelopeMessage se =
  Text.pack (case fromException se of
    Just (ErrorCall m) -> m
    Nothing ->
      case se of
        SomeException inner -> displayException inner)

-- | Shared tail of the internal-error paths: the dedicated exit code.
internalErrorExit :: IO a
internalErrorExit = System.exitWith (toSystemExitCode exitInternalError)

-- | Surface the structured error to @stderr@ (so the user sees the
-- @CODE: message@ line that complements the JSON envelope on stdout).
reportError :: HyphaError -> IO ()
reportError err =
  hPutStrLn stderr $
    Text.unpack (errorCode err) <> ": " <> Text.unpack (errorMessage err)


-- | Compact / full field sets per command name.  Keep in sync with each
-- command module's local key declarations.  Equal sets where there is no
-- distinction yet (alpha).
compactKeysFor, fullKeysFor :: ClientCommandTag -> Set Text
compactKeysFor = \case
  -- tiers_consulted dropped from compact: redundant with per-provider 'tier'.
  LookupCmd   -> Set.fromList ["query", "providers"]
  PackageCmd  -> Package.compactKeys
  VersionsCmd -> Versions.compactKeys
  ModuleCmd   -> Module.compactKeys
  SourceCmd   -> Source.compactKeys
  DoctorCmd   -> Doctor.compactKeys
  DepsCmd     -> Deps.compactKeys
  SymbolCmd   -> Symbol.compactKeys
fullKeysFor = \case
  LookupCmd   -> Set.fromList ["query", "providers", "tiers_consulted"]
  PackageCmd  -> Package.fullKeys
  VersionsCmd -> Versions.fullKeys
  ModuleCmd   -> Module.fullKeys
  SourceCmd   -> Source.fullKeys
  DoctorCmd   -> Doctor.fullKeys
  DepsCmd     -> Deps.fullKeys
  SymbolCmd   -> Symbol.fullKeys
