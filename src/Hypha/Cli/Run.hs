{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Hypha.Cli.Run
  ( -- * Execution
    runCli
    -- * Internals exposed for testing
  , dispatch
  , humanFromValue
  , classifyLookupException
  ) where

import Control.Exception.Safe (IOException, try, SomeException, fromException)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import Crypto.Hash.SHA256 qualified as SHA256
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Key qualified as Key
import Data.Aeson qualified as Aeson
import Data.Aeson (Value)
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.ByteString.Lazy qualified as LBS
import Data.Foldable (for_)
import Data.Map.Strict qualified as Map
import Data.Maybe (maybeToList)
import Data.Set qualified as Set
import Data.Set (Set)
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as TIO
import Data.Text qualified as Text
import Data.Text (Text)
import Data.Vector qualified as V
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Directory
import System.Exit qualified as System
import System.FilePath ((</>), takeDirectory, takeFileName)
import System.IO.Error (isDoesNotExistError)
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import System.Process (readProcessWithExitCode)

import Hypha.BuildEnv.Cabal (CabalStoreError (..), mkCabalBuildEnv)
import Hypha.BuildEnv.Type (BuildEnv (..))
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
import Hypha.Exit (toSystemExitCode)
import Hypha.Hackage.Api (HackageClient, mkHackageClient, mkOfflineHackageClient)
import Hypha.Hoogle.Local qualified as HogLocal
import Hypha.Hoogle.Remote qualified as HogRemote
import Hypha.Logging (LogEvent (..), silentTracer, verboseTracer)
import Hypha.Output.Json (EnvelopeOpts (..), encodeOutcomeBytes, parseSelectList)
import Hypha.Output.Outcome
import Hypha.Package.Resolver
  ( PackageResolver (..), ResolvedPackage (..), mkPackageResolver, resolveRef )
import Hypha.Project.Components qualified as Comp
import Hypha.Project.Discovery (discoverProjectRoot)
import Hypha.Project.Fingerprint qualified as Fingerprint
import Hypha.Project.Overrides (parsePackageOverride)
import Hypha.Project.Plan (loadBuildPlan, planHash)
import Hypha.Search.PackageCache qualified as PC
import Hypha.Source.Modules qualified as SourceModules
import Hypha.Types.BuildPlan
import Hypha.Types.PackageId
  ( PackageName (..), Version (..), PackageId (..), PackageRef (..)
  , parsePackageRef )

-- | Top-level entry point.  Wires global flags and the chosen subcommand to
-- their handlers and emits exactly one JSON envelope (or, with @--human@, a
-- terminal-friendly rendering of the same content).
runCli :: GlobalFlags -> Command -> IO ()
runCli flags cmd = do
  let tracer = if gfVerbose flags then verboseTracer else silentTracer
  tracer (LogInfo "starting hypha")
  case cmd of
    ServerCommands (ServerCommand port mBind prebuild jobs) ->
      runServerInteractive flags port mBind prebuild jobs
    ClientCommands ccmd ->
      processOutcome flags (clientCommandTag ccmd) =<< dispatch flags ccmd

-- | Load the project root + build plan (with overrides applied) inside
-- 'ExceptT'.  Used by command arms that need a typed plan but do not
-- need full resolver/build-env machinery.
loadPlan :: GlobalFlags -> ExceptT HyphaError IO (ProjectRoot, BuildPlan)
loadPlan flags = do
  root      <- discoverProjectRootE (gfProjectDir flags)
  rawPlan   <- loadBuildPlanE root
  overrides <- collectOverridesE (gfPackageOverrides flags)
  pure (root, applyOverrides overrides rawPlan)

-- | Parse the @--package-override@ list inside 'ExceptT'.
collectOverridesE
  :: Monad m => [Text] -> ExceptT HyphaError m [PackageOverride]
collectOverridesE raws = case traverse parsePackageOverride raws of
  Left  err -> throwE (UserError (Text.pack (show err)))
  Right xs  -> pure xs

-- | Create a Hackage client respecting the offline flag.
mkHackageClientForFlags :: GlobalFlags -> IO (HackageClient IO)
mkHackageClientForFlags flags =
  if gfOffline flags
    then mkOfflineHackageClient
    else do
      mgr <- newManager tlsManagerSettings
      mkHackageClient mgr

-- | Run an 'IO' action returning 'Either'; on 'Left', emit a single
-- warning line to @stderr@ and substitute the supplied fallback.  Use
-- at the seams where degraded behaviour is intentional but the
-- underlying failure MUST be visible to the user.  See the
-- "Well-Typed Ethos" entry in @CLAUDE.md@: silent error-branch swallow
-- is banished.
warnOnLeft
  :: (err -> Text)   -- ^ render the error for the warning line
  -> a               -- ^ fallback value substituted on 'Left'
  -> IO (Either err a)
  -> IO a
warnOnLeft renderErr fallback action = action >>= \case
  Right x  -> pure x
  Left err -> do
    hPutStrLn stderr ("warning: " <> Text.unpack (renderErr err))
    pure fallback

-- | Best-effort project + plan loader.  Each failure is announced on
-- @stderr@ before the degraded fallback kicks in — no @Left _ -> ...@
-- silent ignore.  Returns 'Nothing' for the project root when
-- discovery failed, and 'emptyBuildPlan' when plan loading failed.
loadProjectAndPlan :: GlobalFlags -> IO (Maybe ProjectRoot, BuildPlan)
loadProjectAndPlan flags = do
  mRoot <- warnOnLeft
             (errorMessage . DiscoveryFailure)
             Nothing
             (fmap Just <$> discoverProjectRoot (gfProjectDir flags))
  plan <- maybe (pure emptyBuildPlan) loadPlanOrWarn mRoot
  pure (mRoot, plan)
  where
    loadPlanOrWarn root =
      warnOnLeft (errorMessage . PlanFailure root) emptyBuildPlan
                 (loadBuildPlan root)

-- | Pick the right build-env constructor for the loaded project.
-- Project-less calls degrade to a store-only env (still announces the
-- underlying failure inside 'mkBuildEnv' / 'mkBasicBuildEnv').
mkBuildEnvFor :: Maybe ProjectRoot -> BuildPlan -> IO (BuildEnv IO)
mkBuildEnvFor Nothing     _    = mkBasicBuildEnv
mkBuildEnvFor (Just root) plan = mkBuildEnv root plan

-- | Build a resolver and associated build-env.  Degrades gracefully
-- when no project / plan is reachable, but every degradation is
-- announced via 'warnOnLeft' so the user is never left guessing why
-- the answer looks empty.  The only hard-fail branch is malformed
-- @--package-override@ values, surfaced as 'UserError'.
loadResolver
  :: GlobalFlags
  -> ExceptT HyphaError IO (PackageResolver IO, BuildEnv IO)
loadResolver flags = do
  hclient       <- liftIO (mkHackageClientForFlags flags)
  (mRoot, raw)  <- liftIO (loadProjectAndPlan flags)
  overrides     <- collectOverridesE (gfPackageOverrides flags)
  let plan = applyOverrides overrides raw
  liftIO $ do
    env      <- mkBuildEnvFor mRoot plan
    resolver <- mkPackageResolver env hclient plan
    pure (resolver, env)

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
-- an unexpected exit code.
detectGhcVersionFromPath :: IO (Maybe Text)
detectGhcVersionFromPath = do
  r <- try @IO @SomeException
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

-- | Per-command dispatch.  Each arm runs inside 'ExceptT HyphaError IO'
-- so plan loading, resolver wiring, and command execution compose
-- without case-cascades on 'Either'.
dispatch
  :: GlobalFlags -> ClientCommand -> IO (Either HyphaError (Outcome Value))
dispatch flags = runExceptT . dispatchE flags

dispatchE
  :: GlobalFlags -> ClientCommand -> ExceptT HyphaError IO (Outcome Value)
dispatchE flags = \case
  LookupCommand q ->
    ExceptT (runLookupCommand flags q)

  PackageCommand rawArg -> do
    let ref = parsePackageRef rawArg
    (resolver, env) <- loadResolver flags
    rp       <- ExceptT (resolveRef resolver ref)
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
    (resolver, _env) <- loadResolver flags
    eAvail    <- liftIO (fetchVrs resolver pkgName)
    (_, plan) <- loadPlan flags
    pure $ case eAvail of
      Left _         -> Versions.runVersionsPure plan pkgName
      Right versions -> Versions.runVersionsWithAvail plan pkgName versions

  ModuleCommand arg -> do
    (pkgT, modPath)  <- parsePkgMod arg
    (resolver, _env) <- loadResolver flags
    rp <- ExceptT (resolveRef resolver (parsePackageRef pkgT))
    let pid = rpPkgId rp
    d  <- ExceptT (resolveSrc resolver pid)
    oc <- liftIO (Module.runModuleFromDir d pid modPath)
    pure (tagOutsidePlan oc (rpIsOutsidePlan rp))

  SymbolCommand arg -> do
    (resolver, env) <- loadResolver flags
    ExceptT (Symbol.runSymbolWith env resolver arg)

  SourceCommand arg -> do
    (pkgT, modPath, mSym) <- parsePkgModOptSym arg
    runSourceArm flags (parsePackageRef pkgT) modPath mSym

  DepsCommand rawArg reverseMode mDepth -> do
    let PackageRef pkgName _ = parsePackageRef rawArg
    (_, plan) <- loadPlan flags
    liftIO (Deps.runDeps plan pkgName reverseMode mDepth)

  DoctorCommand ->
    liftIO Doctor.runDoctor

-- | Parse @PKG/MOD@ inside 'ExceptT'.
parsePkgMod :: Monad m => Text -> ExceptT HyphaError m (Text, Text)
parsePkgMod arg = case Text.splitOn "/" arg of
  [pkg, modPath] -> pure (pkg, modPath)
  _              -> throwE
    (UserError ("expected PKG/MOD (got: " <> arg <> ")"))

-- | Parse @PKG/MOD[/SYM]@ inside 'ExceptT'.
parsePkgModOptSym
  :: Monad m => Text -> ExceptT HyphaError m (Text, Text, Maybe Text)
parsePkgModOptSym arg = case Text.splitOn "/" arg of
  [pkg, modPath]      -> pure (pkg, modPath, Nothing)
  [pkg, modPath, sym] -> pure (pkg, modPath, Just sym)
  _                   -> throwE
    (UserError ("expected PKG/MOD[/SYM] (got: " <> arg <> ")"))

-- | Server interactive arm.  Refuses non-loopback binds with exit 2; on
-- successful bind it blocks inside Warp until interrupted.
runServerInteractive
  :: GlobalFlags
  -> Int
  -> Maybe Text
  -> Bool
  -> Int
  -> IO ()
runServerInteractive flags port mBind prebuild jobs = do
  result <- runExceptT $ do
    ba              <- bindAddrE port mBind
    (mRoot, plan)   <- liftIO (loadProjectAndPlan flags)
    hclient         <- liftIO (mkHackageClientForFlags flags)
    env             <- liftIO (mkBuildEnvFor mRoot plan)
    resolver        <- liftIO (mkPackageResolver env hclient plan)
    let opts = Server.ServerOpts ba prebuild (fromIntegral (max 1 jobs))
    withExceptT (UserError . Text.pack . renderBindError) $
      ExceptT (Server.runServer mRoot plan env resolver opts)
  case result of
    Left err -> do
      hPutStrLn stderr (Text.unpack (errorMessage err))
      System.exitWith (toSystemExitCode (errorExitCode err))
    Right () -> System.exitSuccess

-- | Resolve the @--bind@ flag inside 'ExceptT'.  Malformed binds map to
-- 'UserError' so the exit code (2) matches user-input failures.
bindAddrE
  :: Monad m
  => Int -> Maybe Text -> ExceptT HyphaError m Server.BindAddr
bindAddrE port mBind =
  withExceptT (UserError . Text.pack . renderBindError) $
    ExceptT (pure (parseBindFromFlags port mBind))

parseBindFromFlags :: Int -> Maybe Text -> Either Server.BindError Server.BindAddr
parseBindFromFlags port = \case
  Just raw -> Server.parseBind raw
  Nothing  -> case Server.portFromInt port of
    Just pn -> Right (Server.defaultBindAddr pn)
    Nothing -> Left (Server.BindMalformed
      (Text.pack ("--port out of range: " <> show port)))

renderBindError :: Server.BindError -> String
renderBindError = \case
  Server.BindMalformed raw   -> "malformed --bind value: " <> Text.unpack raw
  Server.BindNonLoopback raw -> "refusing non-loopback bind: " <> Text.unpack raw

-- | Source command arm: resolve package (plan → store → Hackage), locate
-- source directory (local → Hackage tarball), then extract snippet.
runSourceArm
  :: GlobalFlags
  -> PackageRef
  -> Text
  -> Maybe Text
  -> ExceptT HyphaError IO (Outcome Value)
runSourceArm flags ref modPath mSym = do
  (resolver, env) <- loadResolver flags
  rp  <- ExceptT (resolveRef resolver ref)
  let pid = rpPkgId rp
  dir <- ExceptT (resolveSrc resolver pid)
  oc  <- ExceptT (Source.runSourceFromDir env pid dir modPath mSym)
  pure (tagOutsidePlan oc (rpIsOutsidePlan rp))

-- | Drive the tiered @lookup@ command.  Builds the package cache
-- and project Hoogle handle, then runs the cascade.  Project root
-- discovery is best-effort: outside a cabal project, only the global
-- cache and remote Hoogle are consulted.
runLookupCommand
  :: GlobalFlags
  -> Text
  -> IO (Either HyphaError (Outcome Value))
runLookupCommand flags q = do
  result <- try @IO @SomeException $ do
    mRoot <- warnOnLeft
               (errorMessage . DiscoveryFailure)
               Nothing
               (fmap Just <$> discoverProjectRoot (gfProjectDir flags))
    cache <- PC.openPackageCache mRoot
    dotHypha <- case mRoot of
      Just (ProjectRoot r) -> do
        let d = r </> ".hypha"
        createDirectoryIfMissing True d
        pure d
      Nothing -> do
        x <- getXdgDirectory XdgCache "hypha"
        let d = x </> "no-project"
        createDirectoryIfMissing True d
        pure d
    storeRoot <- defaultStoreRoot
    distRoot  <- defaultDistDocRoot
    hoogleLocal <- HogLocal.openLocalHoogle dotHypha storeRoot distRoot

    -- Bring the local Hoogle DB up to date before the cascade runs.
    -- Without this, Tier 2 always opens an empty/missing .hoo and
    -- every type-signature query falls through to remote Hoogle.
    for_ mRoot $ \root ->
      ensureProjectHoogle storeRoot distRoot dotHypha hoogleLocal root

    let opts = Lookup.LookupOptions
          { Lookup.loOffline = gfOffline flags
          , Lookup.loRemote  =
              HogRemote.defaultRemoteOptions
                { HogRemote.roOffline = gfOffline flags }
          }
    Lookup.runLookup cache hoogleLocal opts q
  case result of
    Left e  -> pure (Left (classifyLookupException e))
    Right o -> pure (Right o)

-- | Classify a 'SomeException' raised inside the @hypha lookup@
-- pipeline.  An @ENOENT@ from a child-process spawn (typically the
-- @haddock@ binary missing on @PATH@, or hidden by a sandbox) becomes
-- 'ToolMissing' so callers can distinguish "this environment lacks a
-- required tool" from "the network died" — and so the CLI exits with
-- the dedicated code instead of pretending it was a network error.
classifyLookupException :: SomeException -> HyphaError
classifyLookupException se
  | Just (ioe :: IOException) <- fromException se
  , isDoesNotExistError ioe
  = ToolMissing (Text.pack (show ioe))
  | otherwise
  = NetworkError (Text.pack (show se))

-- | Materialise the project Hoogle DB: load the plan, derive a
-- 'HoogleStamp' (plan hash + aggregate source-tree fingerprint),
-- enumerate the units, and call 'ensureFresh'.  Failures along the
-- way are swallowed silently — Tier 2 is best-effort, the cascade
-- still works without it.
ensureProjectHoogle
  :: FilePath          -- ^ store root
  -> FilePath          -- ^ dist doc root
  -> FilePath          -- ^ project @.hypha@ directory
  -> HogLocal.HyphaHoogle
  -> ProjectRoot
  -> IO ()
ensureProjectHoogle storeRoot distRoot dotHypha _ root = do
  -- Best-effort: ANY failure here (missing toolchain in a sandbox,
  -- IO errors regenerating the DB, Hoogle library panics) must not
  -- abort the lookup. The cascade in 'Lookup.runLookup' is designed
  -- to fall through to remote Hoogle when Tier 2 yields no hits, so
  -- we swallow the exception and let it proceed.
  _ <- try @IO @SomeException $ do
    ePlan <- loadBuildPlan root
    case ePlan of
      Left _     -> pure ()
      Right plan -> do
        let units = planToLocalUnits plan
            ph    = planHash plan
        fp <- aggregateFingerprint units
        let stamp = HogLocal.HoogleStamp ph fp
        HogLocal.ensureFresh HogLocal.defaultHaddockRunner
                             storeRoot distRoot dotHypha stamp units
  pure ()

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
  r <- try @IO @SomeException (readProcessWithExitCode "ghc"
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
      modules <- SourceModules.getExposedModules dir
      if null modules then trySrcResolver else pure modules
    Nothing -> trySrcResolver
  where
    trySrcResolver :: IO [Text.Text]
    trySrcResolver = do
      eDir <- resolveSrc resolver pid
      case eDir of
        Left _err  -> pure []
        Right dir -> SourceModules.getExposedModules dir

-- | Emit the outcome to stdout, honouring all output-shaping flags.
processOutcome :: GlobalFlags -> ClientCommandTag -> Either HyphaError (Outcome Value) -> IO ()
processOutcome flags cmd result = do
  let oc      = either errorOutcome id result
      compact = compactKeysFor cmd
      full    = fullKeysFor cmd
      opts    = EnvelopeOpts
                  { eoFull       = gfFull flags
                  , eoSelect     = maybe [] parseSelectList (gfSelect flags)
                  , eoPrettyJson = gfPrettyJson flags
                  }
  if gfHuman flags
    then do
      let bs = encodeOutcomeBytes opts cmd compact full oc
      case Aeson.eitherDecode bs of
        Right v -> TIO.putStrLn (humanFromValue v)
        Left  _ -> LBS8.putStrLn bs
    else LBS.hPut stdout (encodeOutcomeBytes opts cmd compact full oc)
  hFlush stdout
  case result of
    Left err -> do
      hPutStrLn stderr $
        Text.unpack (errorCode err) <> ": " <> Text.unpack (errorMessage err)
      System.exitWith (toSystemExitCode (errorExitCode err))
    Right _  -> System.exitSuccess

errorOutcome :: HyphaError -> Outcome Value
errorOutcome = failureOutcome . errorToOutcomeError


-- | Compact / full field sets per command name.  Keep in sync with each
-- command module's local key declarations.  Equal sets where there is no
-- distinction yet (alpha).
compactKeysFor, fullKeysFor :: ClientCommandTag -> Set Text
compactKeysFor = \case
  LookupCmd   -> Set.fromList ["query", "providers", "tiers_consulted"]
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

-- | A small, dependency-free human renderer used by @--human@.  Walks the
-- envelope and prints a readable summary.  This is a stop-gap until the
-- DocH→ANSI renderer lands (Plan A task 11 / issue 017).
humanFromValue :: Value -> Text
humanFromValue (Aeson.Object obj) =
  let cmd      = stringAt obj "command"
      ok       = boolAt   obj "ok"
      outside  = boolAt   obj "outside_plan"
      line0    = "hypha " <> cmd <> (if ok then "" else "  [error]")
      line1    = if outside then "  [outside-plan]" else ""
      body     = renderResult (KM.lookup "result"  obj)
      acts     = renderActions (KM.lookup "actions" obj)
      rel      = renderRelated (KM.lookup "related" obj)
      errBlock = if ok then ""
                 else case KM.lookup "error" obj of
                        Just (Aeson.Object e) ->
                          "\n" <> stringAt e "code" <> ": "
                                <> stringAt e "message"
                        _ -> ""
  in Text.intercalate "\n" $ filter (not . Text.null)
       [ line0 <> line1, errBlock, body, acts, rel ]
humanFromValue other = renderJsonValue 0 other

-- | Look up a 'String' value in an Aeson 'Object', defaulting to empty.
stringAt :: KM.KeyMap Value -> Key -> Text
stringAt obj k = case KM.lookup k obj of
  Just (Aeson.String s) -> s
  _                     -> ""

-- | Look up a 'Bool' value in an Aeson 'Object', defaulting to 'False'.
boolAt :: KM.KeyMap Value -> Key -> Bool
boolAt obj k = case KM.lookup k obj of
  Just (Aeson.Bool b) -> b
  _                   -> False

renderActions :: Maybe Value -> Text
renderActions (Just (Aeson.Object km)) | not (KM.null km) =
  "actions:\n" <> Text.intercalate "\n"
    [ "  " <> Key.toText k <> "  " <> case val of
                                        Aeson.String s -> s
                                        _              -> ""
    | (k, val) <- KM.toList km
    ]
renderActions _ = ""

renderRelated :: Maybe Value -> Text
renderRelated (Just (Aeson.Array xs)) | not (V.null xs) =
  "related:\n" <> Text.intercalate "\n"
    [ case x of
        Aeson.Object o ->
          let lbl = stringAt o "label"
              fch = stringAt o "fetch"
          in "  " <> lbl <> "  " <> fch
        _ -> ""
    | x <- V.toList xs
    ]
renderRelated _ = ""

-- | Tiny indented value printer used as a fallback for the @result@ body.
renderResult :: Maybe Value -> Text
renderResult Nothing  = ""
renderResult (Just v) = "result:\n" <> renderJsonValue 1 v

renderJsonValue :: Int -> Value -> Text
renderJsonValue depth v =
  let ind = Text.replicate (depth * 2) " "
  in case v of
       Aeson.Object km ->
         if KM.null km
         then ind <> "{}"
         else Text.intercalate "\n"
           [ case val of
               -- Non-empty nested objects/arrays rendered on their own lines
               Aeson.Object km2 | not (KM.null km2) -> ind <> Key.toText k <> ":\n" <> renderJsonValue (depth + 1) val
               Aeson.Array  xs  | not (V.null xs)   -> ind <> Key.toText k <> ":\n" <> renderJsonValue (depth + 1) val
               -- Empty objects/arrays and leaf values inline
               _                                     -> ind <> Key.toText k <> ": " <> renderInline val
           | (k, val) <- KM.toList km ]
       Aeson.Array xs ->
         if V.null xs
         then ind <> "[]"
         else Text.intercalate "\n"
           [ ind <> "- " <> renderInline x | x <- V.toList xs ]
       other -> ind <> renderInline other

renderInline :: Value -> Text
renderInline = \case
  Aeson.String s -> s
  Aeson.Number n ->
    let s = Text.pack (show n)
        -- Trim redundant ".0" suffix from whole-number Scientific values
    in if ".0" `Text.isSuffixOf` s then Text.dropEnd 2 s else s
  Aeson.Bool b   -> if b then "true" else "false"
  Aeson.Null     -> "null"
  Aeson.Object km ->
    let entries = [ Key.toText k <> ": " <> renderInline val | (k, val) <- KM.toList km ]
    in "{" <> Text.intercalate ", " entries <> "}"
  Aeson.Array xs ->
    let items = [ renderInline x | x <- V.toList xs ]
    in "[" <> Text.intercalate ", " items <> "]"
