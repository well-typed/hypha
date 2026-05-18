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
  ) where

import Control.Exception (try, SomeException)
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
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import qualified System.Exit as System

import Hypha.BuildEnv.Cabal (mkCabalBuildEnv)
import Hypha.BuildEnv.Type (BuildEnv (..))
import qualified Hypha.Command.Deps       as Deps
import qualified Hypha.Command.Doctor   as Doctor
import qualified Hypha.Command.Module   as Module
import qualified Hypha.Command.Package  as Package
import qualified Hypha.Command.Search        as Search
import qualified Hypha.Command.Source        as Source
import qualified Hypha.Command.Symbol        as Symbol
import qualified Hypha.Command.Versions      as Versions
import qualified Hypha.Command.WhatProvides  as WhatProvides
import Hypha.Cli.Parser (GlobalFlags (..), Command (..))
import Hypha.Error (HyphaError (..), errorCode, errorMessage, errorExitCode, toOutcomeError)
import Hypha.Exit (toSystemExitCode)
import Hypha.Hackage.Api (mkHackageClient, mkOfflineHackageClient)
import Hypha.Hoogle.Query (mkHoogleForFlags)
import Hypha.Logging (LogEvent (..), silentTracer, verboseTracer)
import Hypha.Output.Json (EnvelopeOpts (..), encodeOutcomeBytes, parseSelectList)
import Hypha.Output.Outcome
  ( Outcome (..), OutcomeError (..)
  , failureOutcome
  )
import Hypha.Package.Resolver (PackageResolver (..), ResolvedPackage (..), mkPackageResolver)
import Hypha.Project.Discovery (DiscoveryError (..), discoverProjectRoot)
import Hypha.Project.Overrides (parsePackageOverride)
import Hypha.Project.Plan (PlanError (..), loadBuildPlan)
import Hypha.Types.BuildPlan
  ( BuildPlan (..), CompilerId (..), PackageOverride (..), ProjectRoot (..)
  , applyOverrides, emptyBuildPlan, lookupPackage
  )
import Hypha.Types.PackageId (PackageName (..), Version (..), PackageId (..))

-- | Top-level entry point.  Wires global flags and the chosen subcommand to
-- their handlers and emits exactly one JSON envelope (or, with @--human@, a
-- terminal-friendly rendering of the same content).
runCli :: GlobalFlags -> Command -> IO ()
runCli flags cmd = do
  let tracer = if gfVerbose flags then verboseTracer else silentTracer
  tracer (LogInfo "starting hypha")
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
withResolver
  :: GlobalFlags
  -> ((PackageResolver IO, BuildEnv IO) -> IO (Either HyphaError a))
  -> IO (Either HyphaError a)
withResolver flags k = do
  eRoot <- discoverProjectRoot (gfProjectDir flags)
  case eRoot of
    Left _noProject -> do
      -- No project?  Try a bare resolver with store-only BuildEnv.
      env <- mkBasicBuildEnv
      hclient <- if gfOffline flags
                   then mkOfflineHackageClient
                   else do
                     mgr <- newManager tlsManagerSettings
                     mkHackageClient mgr
      plan <- mkPlanFromPlanJson
      resolver <- mkPackageResolver env hclient plan
      k (resolver, env)
    Right root -> do
      env <- mkRootedBuildEnv root
      hclient <- if gfOffline flags
                   then mkOfflineHackageClient
                   else do
                     mgr <- newManager tlsManagerSettings
                     mkHackageClient mgr
      ePlan <- loadBuildPlan root
      case ePlan of
        Left _planErr -> do
          -- Plan missing; use an empty resolver via store + Hackage
          let emptyPlan = emptyBuildPlan
          resolver <- mkPackageResolver env hclient emptyPlan
          k (resolver, env)
        Right rawPlan -> do
          overrides <- collectOverrides (gfPackageOverrides flags)
          case overrides of
            Left e      -> pure (Left e)
            Right os -> do
              let appliedPlan = applyOverrides os rawPlan
              resolver <- mkPackageResolver env hclient appliedPlan
              k (resolver, env)

-- | Create a basic BuildEnv (store only, no project source dirs).
mkBasicBuildEnv :: IO (BuildEnv IO)
mkBasicBuildEnv = do
  home <- getHomeDirectory
  let ghcDirs = [ home </> ".cabal" </> "store" </> d | d <- ["ghc-9.10.3-d332", "ghc-9.6.7", "ghc-9.6.6"] ]
      storeDir = head ghcDirs
  eEnv <- mkCabalBuildEnv storeDir
  case eEnv of
    Right env -> pure env
    Left _    -> pure offlineNullBuildEnv

-- | BuildEnv rooted at a project directory.
mkRootedBuildEnv :: ProjectRoot -> IO (BuildEnv IO)
mkRootedBuildEnv _root = do
  home <- getHomeDirectory
  -- Try to detect GHC version from a plan, or use a reasonable default
  let storeDir = home </> ".cabal" </> "store" </> "ghc-9.6.7"
  eEnv <- mkCabalBuildEnv storeDir
  case eEnv of
    Right env -> pure env
    Left _    -> pure offlineNullBuildEnv

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
  SearchCommand q extras ->
    runSearchWithHoogle flags q extras

  PackageCommand rawArg ->
    withResolver flags $ \(resolver, _env) -> do
      let (rawName, _mVerHint) = splitVersionHint rawArg
      result <- resolvePkg resolver (PackageName rawName)
      case result of
        Left hyErr -> pure (Left hyErr)
        Right rp ->
          pure (Right (Package.mkSuccessOutcome
            rawName
            (pkgVersion (rpPkgId rp))
            (rpIsLocal rp)
            (rpDepsCount rp)))

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
        withResolver flags $ \(resolver, env) -> do
          let pkgName = PackageName pkg
          result <- resolvePkg resolver pkgName
          case result of
            Left hyErr -> pure (Left hyErr)
            Right rp -> do
              let pid = rpPkgId rp
              oc <- Module.runModule env pid modPath
              pure (Right (tagOutsidePlan oc (rpIsOutsidePlan rp)))
      _ -> pure (Left (UserError ("expected PKG/MOD (got: " <> arg <> ")")))

  SymbolCommand arg ->
    withResolver flags $ \(resolver, env) ->
      Symbol.runSymbolWith env resolver arg

  SourceCommand arg ->
    case Text.splitOn "/" arg of
      [pkg, modPath] ->
        withPlan flags $ \root plan ->
          case lookupPackage (PackageName pkg) plan of
            Nothing -> pure (Left (NotFound
              ("package '" <> pkg <> "' not in build plan")))
            Just ver -> do
              env <- mkBuildEnv root plan
              let pid = PackageId (PackageName pkg) ver
              Source.runSource env plan pid modPath Nothing
      [pkg, modPath, sym] ->
        withPlan flags $ \root plan ->
          case lookupPackage (PackageName pkg) plan of
            Nothing -> pure (Left (NotFound
              ("package '" <> pkg <> "' not in build plan")))
            Just ver -> do
              env <- mkBuildEnv root plan
              let pid = PackageId (PackageName pkg) ver
              Source.runSource env plan pid modPath (Just sym)
      _ -> pure (Left (UserError ("expected PKG/MOD[/SYM] (got: " <> arg <> ")")))

  DepsCommand pkgName reverseMode mDepth ->
    withPlan flags $ \_root plan -> do
      outcome <- Deps.runDeps plan (PackageName pkgName) reverseMode mDepth
      pure (Right outcome)

  WhatProvidesCommand sym -> do
    result <- try @SomeException $ do
      hoogle <- mkHoogleForFlags flags
      WhatProvides.runWhatProvides hoogle sym
    case result of
      Left e  -> pure (Left (NetworkError (Text.pack (show e))))
      Right o -> pure (Right o)

  DoctorCommand ->
    Doctor.runDoctor >>= \outcome -> pure (Right outcome)

-- | Wire the @search@ command to a real Hoogle DB.  Search is the only
-- command that can operate without a plan (it can fall back to the global
-- DB), so we don't go through 'withPlan' here.
runSearchWithHoogle
  :: GlobalFlags
  -> Text
  -> [Text]
  -> IO (Either HyphaError (Outcome Value))
runSearchWithHoogle flags q extras = do
  result <- try @SomeException $ do
    hoogle <- mkHoogleForFlags flags
    Search.runSearchWith hoogle q extras
  case result of
    Left e  -> pure (Left (NetworkError (Text.pack (show e))))
    Right o -> pure (Right o)

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

-- | Set the outside_plan flag on an Outcome.
tagOutsidePlan :: Outcome Value -> Bool -> Outcome Value
tagOutsidePlan (OutcomeSuccess r _ o a rel) flag =
  OutcomeSuccess r flag o a rel
tagOutsidePlan (OutcomeFailure err a) _ = OutcomeFailure err a

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
  "search"       -> Search.compactKeys
  "package"      -> Package.compactKeys
  "versions"     -> Versions.compactKeys
  "module"       -> Module.compactKeys
  "source"       -> Source.compactKeys
  "doctor"       -> Doctor.compactKeys
  "deps"         -> Deps.compactKeys
  "symbol"       -> Symbol.compactKeys
  "whatprovides" -> WhatProvides.compactKeys
  _              -> Set.empty
fullKeysFor = \case
  "search"       -> Search.fullKeys
  "package"      -> Package.fullKeys
  "versions"     -> Versions.fullKeys
  "module"       -> Module.fullKeys
  "source"       -> Source.fullKeys
  "doctor"       -> Doctor.fullKeys
  "deps"         -> Deps.fullKeys
  "symbol"       -> Symbol.fullKeys
  "whatprovides" -> WhatProvides.fullKeys
  _              -> Set.empty

commandName :: Command -> Text
commandName = \case
  SearchCommand _ _      -> "search"
  PackageCommand _       -> "package"
  ModuleCommand _        -> "module"
  SymbolCommand _        -> "symbol"
  SourceCommand _        -> "source"
  VersionsCommand _      -> "versions"
  DepsCommand _ _ _      -> "deps"
  WhatProvidesCommand _  -> "whatprovides"
  DoctorCommand          -> "doctor"

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
