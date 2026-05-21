{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Hypha.Cli.Run
  ( -- * Execution
    runCli
    -- * Internals exposed for testing
  , withPlan
  , dispatch
  , humanFromValue
  , classifyLookupException
  ) where

import Control.Exception (IOException, try, SomeException, fromException)
import System.IO.Error (isDoesNotExistError)
import Data.Aeson (Value)
import qualified Data.Aeson as Aeson
import Data.Aeson.Key (Key)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Directory
  ( XdgDirectory (..), createDirectoryIfMissing, doesDirectoryExist
  , getHomeDirectory, getXdgDirectory, listDirectory )
import System.FilePath ((</>), takeDirectory, takeFileName)
import System.Process (readProcessWithExitCode)
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import qualified System.Exit as System

import Hypha.BuildEnv.Cabal (mkCabalBuildEnv)
import Hypha.BuildEnv.Type (BuildEnv (..))
import qualified Hypha.Command.Deps       as Deps
import qualified Hypha.Command.Doctor   as Doctor
import qualified Hypha.Command.Module   as Module
import qualified Hypha.Command.Package  as Package
import qualified Hypha.Source.Modules   as SourceModules
import qualified Hypha.Command.Lookup        as Lookup
import qualified Hypha.Command.Server        as Server
import qualified Hypha.Command.Source        as Source
import qualified Hypha.Command.Symbol        as Symbol
import qualified Hypha.Command.Versions      as Versions
import Hypha.Cli.Parser (GlobalFlags (..), Command (..))
import Hypha.Error (HyphaError (..), errorCode, errorMessage, errorExitCode, toOutcomeError)
import Hypha.Exit (toSystemExitCode)
import Hypha.Hackage.Api (HackageClient, mkHackageClient, mkOfflineHackageClient)
import qualified Crypto.Hash.SHA256          as SHA256
import qualified Data.ByteString.Base16      as Base16
import Data.Maybe                            (maybeToList)
import qualified Data.Map.Strict             as Map
import qualified Data.Text.Encoding          as Text
import qualified Hypha.Hoogle.Local          as HogLocal
import qualified Hypha.Project.Components    as Comp
import qualified Hypha.Project.Fingerprint   as Fingerprint
import Hypha.Project.Plan                    (planHash)
import Hypha.Types.BuildPlan                 (PlannedUnit (..))
import qualified Hypha.Hoogle.Remote         as HogRemote
import qualified Hypha.Search.PackageCache   as PC
import Hypha.Logging (LogEvent (..), silentTracer, verboseTracer)
import Hypha.Output.Json (EnvelopeOpts (..), encodeOutcomeBytes, parseSelectList)
import Hypha.Output.Outcome
  ( Outcome (..), OutcomeError (..)
  , failureOutcome, tagOutsidePlan
  )
import Hypha.Package.Resolver (PackageResolver (..), ResolvedPackage (..), mkPackageResolver)
import Hypha.Project.Discovery (DiscoveryError (..), discoverProjectRoot)
import Hypha.Project.Overrides (parsePackageOverride)
import Hypha.Project.Plan (PlanError (..), loadBuildPlan)
import Hypha.Types.BuildPlan
  ( BuildPlan (..), CompilerId (..), PackageOverride (..), ProjectRoot (..)
  , applyOverrides, emptyBuildPlan
  )
import Hypha.Types.PackageId (PackageName (..), Version (..), PackageId (..))

-- | Top-level entry point.  Wires global flags and the chosen subcommand to
-- their handlers and emits exactly one JSON envelope (or, with @--human@, a
-- terminal-friendly rendering of the same content).
runCli :: GlobalFlags -> Command -> IO ()
runCli flags cmd = do
  let tracer = if gfVerbose flags then verboseTracer else silentTracer
  tracer (LogInfo "starting hypha")
  case cmd of
    ServerCommand port mBind prebuild jobs ->
      runServerInteractive flags port mBind prebuild jobs
    _ -> do
      result <- dispatch flags cmd
      emit flags (commandName cmd) result
      case result of
        Right{}  -> System.exitWith System.ExitSuccess
        Left err -> System.exitWith (toSystemExitCode (errorExitCode err))

-- | Run the body action with the loaded build plan (project resolution +
-- overrides applied).  Returns an environment error if no plan is reachable.
withPlan
  :: GlobalFlags
  -> (ProjectRoot -> BuildPlan -> IO (Either HyphaError (Outcome Value)))
  -> IO (Either HyphaError (Outcome Value))
withPlan flags k = do
  eRoot <- discoverProjectRoot (gfProjectDir flags)
  case eRoot of
    Left (NoProjectFound where_) ->
      pure (Left (EnvError
        ("no cabal project found (searched up from " <> Text.pack where_ <> ")")))
    Right root -> do
      ePlan <- loadBuildPlan root
      case ePlan of
        Left e -> pure (Left (planErrorToHypha root e))
        Right rawPlan -> do
          overrides <- collectOverrides (gfPackageOverrides flags)
          case overrides of
            Left e   -> pure (Left e)
            Right os -> k root (applyOverrides os rawPlan)

planErrorToHypha :: ProjectRoot -> PlanError -> HyphaError
planErrorToHypha (ProjectRoot r) = \case
  PlanNotFound _msg -> EnvError
    ("plan.json missing under " <> Text.pack r <> "; run `cabal build --dry-run`")
  PlanParseFailure msg -> Corruption ("plan.json parse failure: " <> Text.pack msg)

collectOverrides :: [Text] -> IO (Either HyphaError [PackageOverride])
collectOverrides raws =
  case traverse parsePackageOverride raws of
    Left  err -> pure (Left (UserError (Text.pack (show err))))
    Right xs  -> pure (Right xs)

-- | Build a resolver that can look up packages beyond the plan.
--   Creates the Hackage client, cabal BuildEnv, and wires them together.
-- | Create a Hackage client respecting the offline flag.
mkHackageClientForFlags :: GlobalFlags -> IO (HackageClient IO)
mkHackageClientForFlags flags =
  if gfOffline flags
    then mkOfflineHackageClient
    else do
      mgr <- newManager tlsManagerSettings
      mkHackageClient mgr

-- | Build a resolver that can look up packages beyond the plan.
--   Creates the Hackage client, cabal BuildEnv, and wires them together.
withResolver
  :: GlobalFlags
  -> ((PackageResolver IO, BuildEnv IO) -> IO (Either HyphaError a))
  -> IO (Either HyphaError a)
withResolver flags k = do
  eRoot <- discoverProjectRoot (gfProjectDir flags)
  case eRoot of
    Left _noProject -> do
      -- No project?  Try a bare resolver with store-only BuildEnv.
      env       <- mkBasicBuildEnv
      hclient   <- mkHackageClientForFlags flags
      plan      <- mkPlanFromPlanJson
      resolver  <- mkPackageResolver env hclient plan
      k (resolver, env)
    Right root -> do
      hclient   <- mkHackageClientForFlags flags
      ePlan     <- loadBuildPlan root
      case ePlan of
        Left _planErr -> do
          env       <- mkBasicBuildEnv
          let emptyPlan = emptyBuildPlan
          resolver <- mkPackageResolver env hclient emptyPlan
          k (resolver, env)
        Right rawPlan -> do
          overrides <- collectOverrides (gfPackageOverrides flags)
          case overrides of
            Left e       -> pure (Left e)
            Right os -> do
              let appliedPlan = applyOverrides os rawPlan
              env       <- mkBuildEnv root appliedPlan
              resolver <- mkPackageResolver env hclient appliedPlan
              k (resolver, env)

-- | Create a basic BuildEnv (store only, no project source dirs).
-- Tries a few common GHC store paths and falls back to a null env.
mkBasicBuildEnv :: IO (BuildEnv IO)
mkBasicBuildEnv = do
  home <- getHomeDirectory
  let candidates = [ home </> ".cabal" </> "store" </> d
                   | d <- ["ghc-9.10.3-d332", "ghc-9.6.7", "ghc-9.6.6"]
                   ]
  let tryStore []     = pure offlineNullBuildEnv
      tryStore (p:ps) = do
        eEnv <- mkCabalBuildEnv p
        case eEnv of
          Right env -> pure env
          Left _    -> tryStore ps
  tryStore candidates

-- | Attempt to load a plan.json from CWD for basic version info.
--   Falls back to empty plan if not found.
mkPlanFromPlanJson :: IO BuildPlan
mkPlanFromPlanJson = do
  eRoot <- discoverProjectRoot Nothing
  case eRoot of
    Left _        -> pure emptyBuildPlan
    Right root -> do
      ePlan <- loadBuildPlan root
      case ePlan of
        Left _  -> pure emptyBuildPlan
        Right p -> pure p

-- | Per-command dispatch.  Each arm returns either an error or a successful
-- outcome.
dispatch :: GlobalFlags -> Command -> IO (Either HyphaError (Outcome Value))
dispatch flags = \case
  LookupCommand q ->
    runLookupCommand flags q

  PackageCommand rawArg ->
    withResolver flags $ \(resolver, env) -> do
      let (rawName, _mVerHint) = splitVersionHint rawArg
      result <- resolvePkg resolver (PackageName rawName)
      case result of
        Left hyErr -> pure (Left hyErr)
        Right rp -> do
          modules0 <- resolveExposedModules resolver env (rpPkgId rp)
          pure (Right (Package.mkSuccessOutcome
            rawName
            (pkgVersion (rpPkgId rp))
            (rpIsLocal rp)
            (rpDepsCount rp)
            (rpOrigin rp)
            modules0))

  VersionsCommand pkg ->
    withResolver flags $ \(resolver, _env) -> do
      let pkgName = PackageName pkg
      avResult <- fetchVrs resolver pkgName
      case avResult of
        Left _err ->
          withPlan flags $ \_root plan ->
            pure (Right (Versions.runVersionsPure plan pkgName))
        Right versions ->
          withPlan flags $ \_root plan ->
            pure (Right (Versions.runVersionsWithAvail plan pkgName versions))

  ModuleCommand arg ->
    case Text.splitOn "/" arg of
      [pkg, modPath] ->
        withResolver flags $ \(resolver, _env) -> do
          let pkgName = PackageName pkg
          result <- resolvePkg resolver pkgName
          case result of
            Left hyErr -> pure (Left hyErr)
            Right rp -> do
              let pid = rpPkgId rp
              eDir <- resolveSrc resolver pid
              case eDir of
                Left err -> pure (Left err)
                Right d  -> do
                  oc <- Module.runModuleFromDir d pid modPath
                  pure (Right (tagOutsidePlan oc (rpIsOutsidePlan rp)))
      _ -> pure (Left (UserError ("expected PKG/MOD (got: " <> arg <> ")")))

  SymbolCommand arg ->
    withResolver flags $ \(resolver, env) ->
      Symbol.runSymbolWith env resolver arg

  SourceCommand arg ->
    case Text.splitOn "/" arg of
      [pkg, modPath]      -> runSourceArm flags pkg modPath Nothing
      [pkg, modPath, sym] -> runSourceArm flags pkg modPath (Just sym)
      _ -> pure (Left (UserError ("expected PKG/MOD[/SYM] (got: " <> arg <> ")")))

  DepsCommand pkgName reverseMode mDepth ->
    withPlan flags $ \_root plan -> do
      outcome <- Deps.runDeps plan (PackageName pkgName) reverseMode mDepth
      pure (Right outcome)

  DoctorCommand ->
    Doctor.runDoctor >>= \outcome -> pure (Right outcome)

  ServerCommand{} ->
    pure (Left (UserError "server command is handled in runCli; should not reach dispatch"))

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
  case parseBindFromFlags port mBind of
    Left be -> do
      hPutStrLn stderr (renderBindError be)
      System.exitWith (System.ExitFailure 2)
    Right ba -> do
      let opts = Server.ServerOpts ba prebuild jobs
      e <- withResolver flags $ \(resolver, env) -> do
        eRoot <- discoverProjectRoot (gfProjectDir flags)
        let mRoot = either (const Nothing) Just eRoot
        plan  <- case eRoot of
          Left _    -> pure emptyBuildPlan
          Right rt  -> either (const emptyBuildPlan) id <$> loadBuildPlan rt
        hclient <- mkHackageClientForFlags flags
        r <- Server.runServer mRoot plan env hclient resolver opts
        case r of
          Left be   -> pure (Left (UserError (Text.pack (renderBindError be))))
          Right ()  -> pure (Right ())
      case e of
        Left err -> do
          hPutStrLn stderr (Text.unpack (errorMessage err))
          System.exitWith (toSystemExitCode (errorExitCode err))
        Right () -> System.exitWith System.ExitSuccess

parseBindFromFlags :: Int -> Maybe Text -> Either Server.BindError Server.BindAddr
parseBindFromFlags port = \case
  Nothing   -> Right (Server.BindAddr "127.0.0.1" port)
  Just raw  -> Server.parseBind raw

renderBindError :: Server.BindError -> String
renderBindError = \case
  Server.BindMalformed raw   -> "malformed --bind value: " <> Text.unpack raw
  Server.BindNonLoopback raw -> "refusing non-loopback bind: " <> Text.unpack raw

-- | Source command arm: resolve package (plan → store → Hackage), locate
-- source directory (local → Hackage tarball), then extract snippet.
runSourceArm
  :: GlobalFlags
  -> Text
  -> Text
  -> Maybe Text
  -> IO (Either HyphaError (Outcome Value))
runSourceArm flags pkg modPath mSym =
  withResolver flags $ \(resolver, env) -> do
    eRp <- resolvePkg resolver (PackageName pkg)
    case eRp of
      Left err -> pure (Left err)
      Right rp -> do
        let pid = rpPkgId rp
        eDir <- resolveSrc resolver pid
        case eDir of
          Left err   -> pure (Left err)
          Right dir  -> do
            oc <- Source.runSourceFromDir env pid dir modPath mSym
            pure (fmap (`tagOutsidePlan` rpIsOutsidePlan rp) oc)

-- | Drive the tiered @lookup@ command.  Builds the package cache
-- and project Hoogle handle, then runs the cascade.  Project root
-- discovery is best-effort: outside a cabal project, only the global
-- cache and remote Hoogle are consulted.
runLookupCommand
  :: GlobalFlags
  -> Text
  -> IO (Either HyphaError (Outcome Value))
runLookupCommand flags q = do
  result <- try @SomeException $ do
    eRoot <- discoverProjectRoot (gfProjectDir flags)
    let mRoot = either (const Nothing) Just eRoot
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
    case mRoot of
      Nothing   -> pure ()                  -- no plan, nothing to feed
      Just root ->
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
  _ <- try @SomeException $ do
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
  r <- try @SomeException (readProcessWithExitCode "ghc"
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

-- | Construct a BuildEnv IO from a project root and its build plan.
mkBuildEnv :: ProjectRoot -> BuildPlan -> IO (BuildEnv IO)
mkBuildEnv (ProjectRoot _) plan = do
  home <- getHomeDirectory
  let CompilerId cid = bpCompiler plan
      ghcDir = "ghc-" <> Text.unpack (Text.takeWhileEnd (/= '-') cid)
      storeDir = home </> ".cabal" </> "store" </> ghcDir
  eEnv <- mkCabalBuildEnv storeDir
  case eEnv of
    Right env -> pure env
    Left _    -> pure offlineNullBuildEnv

-- | Empty BuildEnv used when no cabal store is reachable.  All operations
-- return 'Nothing' / empty sets.  GHC version surfaces as a sentinel
-- @"unknown"@ so that callers can detect the absence without crashing.
offlineNullBuildEnv :: BuildEnv IO
offlineNullBuildEnv = BuildEnv
  { discoverInstalledPackages = pure Set.empty
  , locatePackageSource       = \_ -> pure Nothing
  , locateHaddockHtml         = \_ -> pure Nothing
  , ghcVersion                = pure (Version "unknown")
  }

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
emit :: GlobalFlags -> Text -> Either HyphaError (Outcome Value) -> IO ()
emit flags cmd result = do
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
    Left err -> hPutStrLn stderr
                  (Text.unpack (errorCode err) <> ": "
                   <> Text.unpack (errorMessage err))
    Right _  -> pure ()

errorOutcome :: HyphaError -> Outcome Value
errorOutcome err =
  let (code, msg, ec) = toOutcomeError err
  in failureOutcome (OutcomeError code msg ec)

-- | Split @PKG[@VER]@ into its parts.
splitVersionHint :: Text -> (Text, Maybe Text)
splitVersionHint raw =
  case Text.splitOn "@" raw of
    [n]    -> (n, Nothing)
    [n, v] -> (n, Just v)
    (n:_)  -> (n, Nothing)
    []     -> ("", Nothing)

-- | Compact / full field sets per command name.  Keep in sync with each
-- command module's local key declarations.  Equal sets where there is no
-- distinction yet (alpha).
compactKeysFor, fullKeysFor :: Text -> Set Text
compactKeysFor = \case
  "lookup"       -> Set.fromList ["query", "providers", "tiers_consulted"]
  "package"      -> Package.compactKeys
  "versions"     -> Versions.compactKeys
  "module"       -> Module.compactKeys
  "source"       -> Source.compactKeys
  "doctor"       -> Doctor.compactKeys
  "deps"         -> Deps.compactKeys
  "symbol"       -> Symbol.compactKeys
  _              -> Set.empty
fullKeysFor = \case
  "lookup"       -> Set.fromList ["query", "providers", "tiers_consulted"]
  "package"      -> Package.fullKeys
  "versions"     -> Versions.fullKeys
  "module"       -> Module.fullKeys
  "source"       -> Source.fullKeys
  "doctor"       -> Doctor.fullKeys
  "deps"         -> Deps.fullKeys
  "symbol"       -> Symbol.fullKeys
  _              -> Set.empty

commandName :: Command -> Text
commandName = \case
  LookupCommand _        -> "lookup"
  PackageCommand _       -> "package"
  ModuleCommand _        -> "module"
  SymbolCommand _        -> "symbol"
  SourceCommand _        -> "source"
  VersionsCommand _      -> "versions"
  DepsCommand _ _ _      -> "deps"
  DoctorCommand          -> "doctor"
  ServerCommand{}        -> "server"

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
