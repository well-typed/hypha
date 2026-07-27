{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | The @hypha server@ subcommand.
--
-- Boots a local doc browser (Warp + WAI) bound to loopback only.  Optional
-- prebuild stage walks the build plan and renders Haddocks concurrently so
-- the first request lands on a warm cache.
module Hypha.Command.Server
  ( -- * Types
    ServerOpts (..)
  , module Hypha.Server.Bind
    -- * Entry points
  , runServer
  , buildServerConfig
    -- * Internals exposed for testing
  , briefException
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Stream (mapConcurrentlyBounded)
import Control.Exception (SomeException (..), displayException, fromException, ErrorCall (..))
import Control.Exception.Safe (try)
import Control.Monad
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
import Control.Monad.Trans.Maybe (MaybeT (..), hoistMaybe, runMaybeT)
import Data.ByteString.Lazy qualified as LBS
import Data.IORef qualified as IORef
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Ord (Down (..))
import Data.String qualified as String
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as TIO
import Data.Text qualified as Text
import Data.Text (Text)
import GHC.Natural (Natural)
import Hypha.BuildEnv.Type (BuildEnv (..))
import Hypha.Haddock.Generate (ensureHaddockFor)
import Hypha.Package.Resolver ( PackageResolver (..), ResolvedPackage (..) )
import Hypha.Project.Components qualified as Comp
import Hypha.Search.Fuzzy qualified as Fuzzy
import Hypha.Search.Indexer qualified as Indexer
import Hypha.Search.PackageCache qualified as Cache
import Hypha.Server.App qualified as App
import Hypha.Server.Bind
import Hypha.Server.Haddock.Extract qualified as HExtract
import Hypha.Server.Haddock.Rewrite qualified as Rewrite
import Hypha.Server.ModuleDoc
import Hypha.Server.Slots qualified as Slots
import Hypha.Source.Extract qualified as Extract
import Hypha.Source.Locate qualified as Locate
import Hypha.Source.Parser qualified as Parser
import Hypha.Types.BuildPlan
import Hypha.Types.ComponentName
import Hypha.Types.Doc (DocText (..))
import Hypha.Types.PackageId
import Network.Wai.Handler.Warp ( defaultSettings, runSettings, setHost, setPort )
import System.Directory qualified as Dir
import System.FilePath qualified as FP
import System.IO (hPutStrLn, stderr)

-- | All @hypha server@ options.
data ServerOpts = ServerOpts
  { soBind         :: !BindAddr
    -- ^ Resolved bind address.
  , soPrebuild     :: !Bool
    -- ^ Walk plan and pre-render Haddocks.
  , soPrebuildJobs :: !Natural
    -- ^ Max concurrent prebuild workers.
  , soCacheRoot   :: !FilePath
    -- ^ Cache root directory (for Haddock cache).
  }
  deriving stock (Show, Eq)

-- | Boot the server.  Returns 'Left' on bind refusal; otherwise blocks
-- inside Warp's event loop.
runServer
  :: Maybe ProjectRoot
  -> BuildPlan
  -> BuildEnv IO
  -> PackageResolver IO
  -> ServerOpts
  -> IO (Either BindError ())
runServer mRoot plan env resolver opts = do
  cfg <- buildServerConfig (soCacheRoot opts) mRoot plan env resolver
  let bind = soBind opts
  hPutStrLn stderr ("hypha server listening on " <> renderBindUrl bind)
  when (soPrebuild opts) $
    prebuildAll (soCacheRoot opts) plan env (soPrebuildJobs opts) (planPackageIds plan)
  let settings = setHost (String.fromString (show (baIP bind)))
               $ setPort (fromIntegral (baPort bind))
                 defaultSettings
  runSettings settings (App.appWith cfg)
  pure (Right ())

-- | Concurrently warm the Haddock cache for every package in the plan.
-- Bounded by a worker pool of 'soPrebuildJobs' threads.  Every
-- per-package outcome is collected and reported on @stderr@ after the
-- pool drains — neither exceptions nor empty haddock results are
-- silently swallowed.
prebuildAll :: FilePath -> BuildPlan -> BuildEnv IO -> Natural -> [PackageId] -> IO ()
prebuildAll cacheRoot plan env jobs pids = do
  outcomes <- mapConcurrentlyBounded (max 1 (fromEnum jobs)) ensureOne pids
  reportPrebuildOutcomes outcomes
  where
    ensureOne :: PackageId -> IO (PackageId, Either SomeException (Maybe FilePath))
    ensureOne pid = do
      r <- try (ensureHaddockFor cacheRoot plan env pid)
      pure (pid, r)

-- | Walk every '(pid, outcome)' returned by 'prebuildAll' and surface
-- the interesting ones.  Concurrent stderr writes are linearised here
-- after the pool has drained so the log isn't interleaved.
reportPrebuildOutcomes
  :: [(PackageId, Either SomeException (Maybe FilePath))] -> IO ()
reportPrebuildOutcomes outcomes = do
  mapM_ logFailure [ (pid, e) | (pid, Left  e)       <- outcomes ]
  mapM_ logMissing [  pid     | (pid, Right Nothing) <- outcomes ]
  where
    logFailure (pid, e) = hPutStrLn stderr $
      "warning: prebuild failed for " <> renderPkgId pid
        <> ": " <> Text.unpack (briefException e)
    logMissing pid      = hPutStrLn stderr
      ("note: prebuild produced no haddock for " <> renderPkgId pid)

-- | Render a 'PackageId' as @<pkg>-<version>@ for log lines.
renderPkgId :: PackageId -> String
renderPkgId (PackageId (PackageName n) (Version v)) =
  Text.unpack n <> "-" <> Text.unpack v

-- | Render a caught exception as one tidy line for a @warning:@/@note:@
-- log message.
--
-- On GHC \>= 9.10 'error' attaches a 'CallStack' in two places: as a
-- legacy location string inside 'ErrorCall', and as a 'Backtraces'
-- annotation in the 'ExceptionContext' carried by 'SomeException'.
-- Both are pure noise for the failures this module catches (a cpphs
-- @#error@ hitting a build-time-only macro, a Haddock subprocess
-- misbehaving) — the failure is either permanent and non-actionable or
-- already identified by the package\/module label the caller prepends.
--
-- Rather than string-matching on the rendered output (fragile — an
-- @error "CallStack overflow"@ message would be truncated), we extract
-- the message structurally:
--
-- 1. For 'ErrorCall' (what 'error' throws), 'fromException' + the
--    'ErrorCall' pattern synonym yields just the message string,
--    discarding the legacy location on /all/ GHC versions.
-- 2. For every other exception type, pattern matching on
--    'SomeException' drops the 'ExceptionContext' (and thus the
--    'Backtraces' annotation) on GHC \>= 9.10; on older GHC the context
--    does not exist, so the match is a harmless no-op.
--
-- What remains is collapsed to a single line.
briefException :: SomeException -> Text
briefException se =
  Text.unwords (Text.words (Text.pack msg))
  where
    msg = case fromException se of
      Just (ErrorCall m) -> m
      Nothing ->
        case se of
          SomeException e -> displayException e

-- | Extract every distinct 'PackageId' from a plan.
planPackageIds :: BuildPlan -> [PackageId]
planPackageIds = map puId . Map.elems . bpUnits

-- | Assemble the 'ServerConfig' callbacks that connect the WAI app to the
-- resolver, build env, and Hoogle.
buildServerConfig
  :: FilePath
  -> Maybe ProjectRoot
  -> BuildPlan
  -> BuildEnv IO
  -> PackageResolver IO
  -> IO App.ServerConfig
buildServerConfig cacheRoot mRoot plan env resolver = do
  let pids     = planPackageIds plan
      packages = concatMap (componentNames plan) pids
  slots <- Slots.initialiseSlots pids
  -- Build a cheap in-memory index from the plan packages' module exports.
  -- Done once asynchronously after startup so the first request lands fast.
  -- We deliberately do NOT consult Hoogle for live search: the per-project
  -- DB generation step fails when no @.txt@ inputs exist and the upstream
  -- library deadlocks under concurrent retry.
  indexRef    <- IORef.newIORef ([] :: [Fuzzy.IndexedRow])
  readyRef    <- IORef.newIORef False
  doneRef     <- IORef.newIORef (0 :: Int)
  totalRef    <- IORef.newIORef (0 :: Int)
  cache       <- Cache.openPackageCache mRoot
  -- Hydrate from the on-disk cache synchronously before the server
  -- accepts requests so the common (warm) path renders results
  -- immediately on the first keystroke.  Anything not yet cached gets
  -- built in the background and persisted for next time.
  missing  <- Indexer.hydrateFromCache plan cache pids indexRef
  IORef.writeIORef totalRef (length missing)
  case missing of
    [] -> IORef.writeIORef readyRef True
    _  -> do
      _ <- forkIO $ do
        r <- try (Indexer.buildAndCacheIndex plan cache resolver missing indexRef doneRef)
        case r :: Either SomeException () of
          Left e  -> hPutStrLn stderr ("hypha index build failed: " <> show e)
          Right _ -> pure ()
        IORef.writeIORef readyRef True
      pure ()
  pure App.ServerConfig
    { App.scProjectName  = projectName plan
    , App.scPackages     = packages
    , App.scSlots        = slots
    , App.scIndexReady   = IORef.readIORef readyRef
    , App.scIndexProgress = (,)
        <$> IORef.readIORef doneRef
        <*> IORef.readIORef totalRef
    , App.scHumanSearch  = \q -> do
        let tokens = Fuzzy.tokenize q
        if null tokens
          then pure []
          else do
            idx <- IORef.readIORef indexRef
            let scored =
                  [ (s, Fuzzy.displayRow row)
                  | row <- idx
                  , Just s <- [Fuzzy.scoreRow tokens row]
                  ]
                ranked = map snd (sortOn (Down . fst) scored)
            pure (take 50 ranked)
    , App.scSymbolLookup = \pkgT modT symT -> do
        mDirs <- resolveComponentDirs plan resolver pkgT
        case mDirs of
          Nothing                -> pure Nothing
          Just (parentDir, dirs) -> do
            mFile <- Locate.findModuleFileIn dirs modT
            case mFile of
              Nothing -> pure Nothing
              Just f  -> do
                src <- TIO.readFile f
                let info0 = Extract.extractSymbolInfo src symT
                -- Re-exports define the symbol elsewhere in the same
                -- package; locateSymbolDefinitionInDir sweeps the
                -- tree ranked by module-path prefix.  When the
                -- module we landed on doesn't actually contain the
                -- binding (sig/haddock came back empty), re-extract
                -- from the file that does so the symbol card isn't
                -- a blank cream box.  We prefer the signature line
                -- as the source anchor whenever it is available: it
                -- sits above any CPP @#ifdef@ branches, so it is
                -- the most faithful target for symbols whose body
                -- is fanned out across platform-specific branches.
                mLoc <- Locate.locateSymbolDefinitionInDir parentDir modT symT
                (info, resolvedMod, lineOverride) <-
                  case (Extract.siSignature info0, mLoc) of
                    (Nothing, Just loc) | Locate.slPath loc /= f -> do
                      src' <- TIO.readFile (Locate.slPath loc)
                      let info' = Extract.extractSymbolInfo src' symT
                          modT' = modulePathFromFile parentDir (Locate.slPath loc)
                      pure (info', modT', Just (Locate.slLine loc))
                    _ -> pure (info0, modT, Nothing)
                let sig = fromMaybe "" (Extract.siSignature info)
                    hd  = maybe "" unDocText (Extract.siHaddock  info)
                    mLine = case (Extract.siSigLine info, Extract.siLine info, lineOverride) of
                      (Just n, _, _)        -> Just n
                      (Nothing, Just n, _)  -> Just n
                      (Nothing, Nothing, l) -> l
                pure (Just SymbolCardData
                  { scdSignature = sig
                  , scdHaddock   = hd
                  , scdModule    = resolvedMod
                  , scdLine      = mLine
                  , scdKind      = Extract.siKind info
                  })
    , App.scHaddockFile  = \pkgVer segments -> runMaybeT $ do
        -- Resolve through the full chain (hypha cache → local dist-dir
        -- → store) instead of assuming the hypha cache, so prebuilt
        -- docs are served from wherever they actually live.
        pid <- hoistMaybe (parsePkgVer pkgVer)
        idx <- MaybeT (ensureHaddockFor cacheRoot plan env pid)
        let dir  = FP.takeDirectory idx
            path = FP.joinPath (dir : map Text.unpack segments)
        exists <- lift (Dir.doesFileExist path)
        guard exists
        bytes <- lift (LBS.readFile path)
        let payload
              | FP.takeExtension path == ".html" =
                  LBS.fromStrict . Text.encodeUtf8
                    . Rewrite.rewriteHaddockHtml
                    . Text.decodeUtf8 . LBS.toStrict $ bytes
              | otherwise = bytes
        pure (path, payload)
    , App.scSourceText   = \pkgT modT -> do
        mDirs <- resolveComponentDirs plan resolver pkgT
        case mDirs of
          Nothing          -> pure Nothing
          Just (_, dirs)   -> do
            mFile <- Locate.findModuleFileIn dirs modT
            case mFile of
              Nothing -> pure Nothing
              Just f  -> Just <$> TIO.readFile f
    , App.scPackageInfo  = \pkgT -> do
        let cn   = parseComponentName pkgT
        ePid <- resolvePkg resolver (cnPackage cn)
        case ePid of
          Left _   -> pure Nothing
          Right rp -> do
            let pid    = rpPkgId rp
                ver    = unVersion (pkgVersion pid)
                origin = rpOrigin rp
            mDirs <- resolveComponentDirs plan resolver pkgT
            case mDirs of
              Nothing        -> pure (Just (ver, [], origin))
              Just (_, dirs) -> do
                mods <- Indexer.enumModulesIn dirs
                pure (Just (ver, mods, origin))
    , App.scModuleDoc = moduleDocFor cacheRoot plan env resolver
    }

-- | The documentation-priority chain for a module page (see
-- 'ModuleDocView'):
--
-- 1. Prebuilt Haddock — resolved via 'ensureHaddockFor' (hypha cache →
--    local dist-dir → store); the module's HTML page is sliced into
--    embeddable regions and its links rewritten.
-- 2. On-the-fly source rendering — one 'Extract.extractModuleDoc' pass
--    over the module source, entries filtered\/ordered by the export
--    list when one parses.
-- 3. Export names + the reason we could not do better; the reason is
--    also traced to stderr, never swallowed.
moduleDocFor
  :: FilePath
  -> BuildPlan
  -> BuildEnv IO
  -> PackageResolver IO
  -> Text            -- ^ component name from the URL
  -> Text            -- ^ dotted module path
  -> IO ModuleDocView
moduleDocFor cacheRoot plan env resolver pkgT modT = do
  mHad <- haddockLocation
  mPre <- case mHad of
    Nothing        -> pure Nothing
    Just (pv, dir) -> prebuiltView pv dir
  case mPre of
    Just v  -> pure v
    Nothing -> sourceView (fst <$> mHad)
  where
    cn = parseComponentName pkgT

    -- @(pkg-ver, haddock dir)@ when rendered docs exist anywhere.
    haddockLocation :: IO (Maybe (Text, FilePath))
    haddockLocation = runMaybeT $ do
      rp  <- MaybeT (either (const Nothing) Just <$> resolvePkg resolver (cnPackage cn))
      idx <- MaybeT (ensureHaddockFor cacheRoot plan env (rpPkgId rp))
      pure (renderPackageId (rpPkgId rp), FP.takeDirectory idx)

    prebuiltView :: Text -> FilePath -> IO (Maybe ModuleDocView)
    prebuiltView pv dir = runMaybeT $ do
      let file = dir FP.</> (Text.unpack (Text.replace "." "-" modT) <> ".html")
      exists <- lift (Dir.doesFileExist file)
      guard exists
      html  <- lift (TIO.readFile file)
      parts <- hoistMaybe (HExtract.extractModuleDocHtml html)
      let ctx = Rewrite.EmbedContext { Rewrite.ecComponent = pkgT
                                     , Rewrite.ecPkgVer    = pv }
          rw  = Rewrite.rewriteEmbeddedDocHtml ctx
      pure $ ViewPrebuilt PrebuiltDoc
        { pdPkgVer      = pv
        , pdDescription = rw <$> HExtract.ppDescription parts
        , pdInterface   = rw (HExtract.ppInterface parts)
        , pdContents    = rw <$> HExtract.ppContents parts
        }

    -- On failure carries (reason, export names best-effort) so the
    -- last-resort view still lists something useful.
    sourceView :: Maybe Text -> IO ModuleDocView
    sourceView mPv = do
      r <- runExceptT $ do
        (_, dirs) <- liftMaybeReason "package source could not be resolved"
                       (resolveComponentDirs plan resolver pkgT)
        f   <- liftMaybeReason
                 ("module " <> modT <> " has no source file in the package")
                 (Locate.findModuleFileIn dirs modT)
        src <- lift (TIO.readFile f)
        case Extract.extractModuleDoc f src of
          Left perr -> throwE
            ( "module source could not be parsed: "
                <> Parser.parseErrorMessage perr
            , Locate.parseExports src
            )
          Right info -> pure (filterByExports src info)
      case r of
        Right info -> pure (ViewFromSource (SourceDoc info mPv))
        Left (reason, names) -> do
          hPutStrLn stderr $
            "hypha server: module docs degraded for "
              <> Text.unpack pkgT <> "/" <> Text.unpack modT
              <> ": " <> Text.unpack reason
          pure (ViewExportsOnly names reason)

    liftMaybeReason
      :: Text -> IO (Maybe a) -> ExceptT (Text, [Text]) IO a
    liftMaybeReason reason act =
      ExceptT (maybe (Left (reason, [])) Right <$> act)

    -- Restrict and order entries by the explicit export list when one
    -- parses.  A filter that would empty the page (pure re-export
    -- modules) keeps the full entry list instead — over-inclusion is
    -- harmless, an empty doc page is not.
    filterByExports :: Text -> Extract.ModuleDocInfo -> Extract.ModuleDocInfo
    filterByExports src info =
      case Locate.parseExports src of
        []   -> info
        exps ->
          let byName = Map.fromList
                [ (Extract.deName e, e) | e <- Extract.mdiEntries info ]
              ordered = mapMaybe (`Map.lookup` byName) exps
          in case ordered of
               [] -> info
               _  -> info { Extract.mdiEntries = ordered }

-- | Resolve a composite component name (e.g. @hypha:lib-foo@) into the
-- parent package's source dir + the component's source-root list.  The
-- parent dir is what 'Locate.locateSymbolDefinitionInDir' wants for
-- re-export sweeps; the source roots are what 'findModuleFileIn' wants
-- for the initial module lookup.  Returns 'Nothing' when the package
-- can't be resolved or the named sublib doesn't exist.
resolveComponentDirs
  :: BuildPlan
  -> PackageResolver IO
  -> Text                          -- ^ raw composite name from URL
  -> IO (Maybe (FilePath, [FilePath]))
resolveComponentDirs plan resolver raw = do
  let cn = parseComponentName raw
  ePid <- resolvePkg resolver (cnPackage cn)
  case ePid of
    Left _   -> pure Nothing
    Right rp -> do
      eDir <- resolveSrc resolver (rpPkgId rp)
      case eDir of
        Left _  -> pure Nothing
        Right d -> do
          let mDirs = case lookupUnit (cnPackage cn) plan of
                Just pu | not (null (puLibComponents pu)) ->
                  case [ Comp.ciHsSourceDirs c
                       | c <- puLibComponents pu
                       , Comp.ciKind c == cnKind cn ] of
                    (xs : _) -> Just xs
                    []       -> Nothing
                _ -> Nothing
          case mDirs of
            Just dirs -> pure (Just (d, dirs))
            Nothing | cnKind cn == Comp.MainLib -> do
              -- Fallback for main-lib references in packages whose
              -- cabal we couldn't parse.
              roots <- Indexer.chooseSourceRoots d
              pure (Just (d, roots))
            Nothing -> pure Nothing

-- | Compute the cache key for one library or executable component.
--
--   * 'MainLib' → bare package name.
--   * 'SubLib s' → @pkg:s@.
--   * 'Exe s'    → @pkg:exe:s@.
componentKey :: Text -> Comp.ComponentKind -> Text
componentKey pkgT Comp.MainLib    = pkgT
componentKey pkgT (Comp.SubLib s) = pkgT <> ":" <> s
componentKey pkgT (Comp.Exe    s) = pkgT <> ":exe:" <> s


-- | Every renderable component name for a unit.  Falls back to a
-- single @pkg@ entry when no components were parsed.
componentNames :: BuildPlan -> PackageId -> [(Text, PackageOrigin)]
componentNames plan pid =
  let pkgT   = unPackageName (pkgName pid)
      origin = case lookupUnit (pkgName pid) plan of
        Just pu -> puOrigin pu
        Nothing -> OriginDistribution
      tag t  = (t, origin)
  in case lookupUnit (pkgName pid) plan of
       Just pu | not (null (puLibComponents pu)) ->
         [ tag (componentKey pkgT (Comp.ciKind c)) | c <- puLibComponents pu ]
       _ -> [tag pkgT]

-- | Recover a module path from an absolute file path resolved inside a
-- package source tree.  Strips the package root, common @hs-source-dirs@
-- prefixes ("src", "library", "lib") and the @.hs@ suffix.
modulePathFromFile :: FilePath -> FilePath -> Text
modulePathFromFile root path =
  let rel0  = case Text.stripPrefix (Text.pack root) (Text.pack path) of
                Just r  -> Text.dropWhile (== '/') r
                Nothing -> Text.pack path
      rel   = stripDirPrefix rel0
      withoutHs = case Text.stripSuffix ".hs" rel of
                    Just r  -> r
                    Nothing -> rel
  in Text.replace "/" "." withoutHs
  where
    stripDirPrefix t = case dropPrefix "src/" t of
      Just r  -> r
      Nothing -> case dropPrefix "library/" t of
        Just r  -> r
        Nothing -> case dropPrefix "lib/" t of
          Just r  -> r
          Nothing -> t
    dropPrefix p = Text.stripPrefix (Text.pack p)


-- | Project name (best-effort).  Uses the first local package, or a
-- placeholder when none are present.
projectName :: BuildPlan -> Text
projectName plan =
  case filter puIsLocal (Map.elems (bpUnits plan)) of
    (pu : _) -> unPackageName (pkgName (puId pu))
    []       -> "hypha"

-- | Parse @"<pkg>-<ver>"@.  The version is the suffix after the last @-@.
parsePkgVer :: Text -> Maybe PackageId
parsePkgVer raw =
  case Text.breakOnEnd "-" raw of
    (pre, ver) | not (Text.null pre) && not (Text.null ver) ->
      let name = Text.dropEnd 1 pre
      in Just (PackageId (PackageName name) (Version ver))
    _ -> Nothing

