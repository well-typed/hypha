{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | The @hypha server@ subcommand.
--
-- Boots a local doc browser (Warp + WAI) bound to loopback only.  Optional
-- prebuild stage walks the build plan and renders Haddocks concurrently so
-- the first request lands on a warm cache.
module Hypha.Command.Server
  ( -- * Types
    ServerOpts (..)
  , BindAddr (..)
  , BindError (..)
    -- * Bind parsing
  , parseBind
    -- * Entry points
  , runServer
  , buildServerConfig
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.QSem (newQSem, signalQSem, waitQSem)
import Control.Concurrent.Async (mapConcurrently_)
import Control.Exception (SomeException, bracket_, try)
import qualified Data.IORef as IORef
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as TIO
import Network.Wai.Handler.Warp
  ( defaultSettings, runSettings, setHost, setPort )
import qualified Data.String as String
import System.IO (hPutStrLn, stderr)

import Hypha.BuildEnv.Type (BuildEnv (..))
import Hypha.Hackage.Api (HackageClient (..))
import Hypha.Haddock.Generate (ensureHaddockFor, haddockDirFor)
import Hypha.Hoogle.Type (Hoogle)
import Hypha.Package.Resolver
  ( PackageResolver (..), ResolvedPackage (..) )
import Data.List (sortOn)
import Data.Ord (Down (..))
import qualified Hypha.Project.Components as Comp
import qualified Hypha.Search.PackageCache as Cache
import Hypha.Search.PackageCache (CacheOrigin (..))
import qualified Hypha.Search.Fuzzy as Fuzzy
import qualified Hypha.Server.App as App
import qualified Hypha.Server.Haddock.Rewrite as Rewrite
import qualified Hypha.Server.Slots as Slots
import qualified Hypha.Source.Extract as Extract
import qualified Hypha.Source.Locate as Locate
import Hypha.Types.BuildPlan
  ( BuildPlan (..), PackageOrigin (..), PlannedUnit (..), ProjectRoot
  , lookupUnit )
import Hypha.Types.ComponentName
  ( ComponentName (..), cnKind, cnPackage, parseComponentName )
import Hypha.Types.Doc (DocText (..))
import Hypha.Types.PackageId
  ( PackageId (..), PackageName (..), Version (..) )
import qualified System.Directory as Dir
import qualified System.FilePath as FP

-- | Bind address.  Loopback only — explicit guard in 'parseBind'.
data BindAddr = BindAddr
  { baHost :: !String
  , baPort :: !Int
  }
  deriving stock (Show, Eq)

-- | Parse failure or refusal.
data BindError
  = BindMalformed   !Text  -- ^ Could not parse @HOST:PORT@.
  | BindNonLoopback !Text  -- ^ Caller asked for a non-loopback bind.
  deriving stock (Show, Eq)

-- | All @hypha server@ options.
data ServerOpts = ServerOpts
  { soBind         :: !BindAddr
    -- ^ Resolved bind address.
  , soPrebuild     :: !Bool
    -- ^ Walk plan and pre-render Haddocks.
  , soPrebuildJobs :: !Int
    -- ^ Max concurrent prebuild workers.
  }
  deriving stock (Show, Eq)

-- | Parse a bind string of the form @HOST:PORT@.  Only loopback hosts are
-- accepted — anything else returns 'BindNonLoopback'.
parseBind :: Text -> Either BindError BindAddr
parseBind raw =
  case Text.splitOn ":" raw of
    [h, p] | Just port <- readPortMaybe (Text.unpack p) ->
      if isLoopback h
        then Right (BindAddr (Text.unpack h) port)
        else Left  (BindNonLoopback raw)
    _ -> Left (BindMalformed raw)
  where
    isLoopback h = h == "localhost" || h == "127.0.0.1" || h == "::1"
    readPortMaybe s = case reads s of
      [(n, "")] | n >= 1 && n <= 65535 -> Just n
      _                                -> Nothing

-- | Boot the server.  Returns 'Left' on bind refusal; otherwise blocks
-- inside Warp's event loop.
runServer
  :: Maybe ProjectRoot
  -> BuildPlan
  -> BuildEnv IO
  -> HackageClient IO
  -> PackageResolver IO
  -> Hoogle IO
  -> ServerOpts
  -> IO (Either BindError ())
runServer mRoot plan env hclient resolver hoogle opts = do
  cfg <- buildServerConfig mRoot plan env hclient resolver hoogle
  hPutStrLn stderr
    ( "hypha server listening on http://" <> baHost (soBind opts)
   <> ":" <> show (baPort (soBind opts))
    )
  case soPrebuild opts of
    False -> pure ()
    True  -> prebuildAll plan env (soPrebuildJobs opts) (planPackageIds plan)
  let settings = setHost (String.fromString (baHost (soBind opts)))
               $ setPort (baPort (soBind opts))
                 defaultSettings
  runSettings settings (App.appWith cfg)
  pure (Right ())

-- | Concurrently warm the Haddock cache for every package in the plan.
prebuildAll :: BuildPlan -> BuildEnv IO -> Int -> [PackageId] -> IO ()
prebuildAll plan env jobs pids = do
  sem <- newQSem (max 1 jobs)
  mapConcurrently_ (withSem sem . ensureOne) pids
  where
    ensureOne pid = do
      r <- try (ensureHaddockFor plan env pid) :: IO (Either SomeException (Maybe FilePath))
      case r of
        Right (Just _) -> pure ()
        _              -> pure ()
    withSem sem action = bracket_ (waitQSem sem) (signalQSem sem) action

-- | Extract every distinct 'PackageId' from a plan.
planPackageIds :: BuildPlan -> [PackageId]
planPackageIds = map puId . Map.elems . bpUnits

-- | Assemble the 'ServerConfig' callbacks that connect the WAI app to the
-- resolver, build env, and Hoogle.
buildServerConfig
  :: Maybe ProjectRoot
  -> BuildPlan
  -> BuildEnv IO
  -> HackageClient IO
  -> PackageResolver IO
  -> Hoogle IO
  -> IO App.ServerConfig
buildServerConfig mRoot plan _env _hclient resolver _hoogle = do
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
  missing  <- hydrateFromCache plan cache pids indexRef
  IORef.writeIORef totalRef (length missing)
  case missing of
    [] -> IORef.writeIORef readyRef True
    _  -> do
      _ <- forkIO $ do
        r <- try (buildAndCacheIndex plan cache resolver missing indexRef doneRef)
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
                let sig = maybe "" id (Extract.siSignature info)
                    hd  = maybe "" unDocText (Extract.siHaddock  info)
                    mLine = case (Extract.siSigLine info, Extract.siLine info, lineOverride) of
                      (Just n, _, _)        -> Just n
                      (Nothing, Just n, _)  -> Just n
                      (Nothing, Nothing, l) -> l
                pure (Just (sig, hd, resolvedMod, mLine))
    , App.scHaddockHtml  = \pkgVer segments -> do
        let pidM = parsePkgVer pkgVer
        case pidM of
          Nothing  -> pure Nothing
          Just pid -> do
            dir <- haddockDirFor pid
            let path = foldl (FP.</>) dir segments
            exists <- Dir.doesFileExist path
            if not exists
              then pure Nothing
              else do
                bs <- LBS.readFile path
                let txt = Text.decodeUtf8 (LBS.toStrict bs)
                pure (Just (LBS.fromStrict (Text.encodeUtf8 (Rewrite.rewriteHaddockHtml txt))))
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
                mods <- enumModulesIn dirs
                pure (Just (ver, mods, origin))
    , App.scModuleExports = \pkgT modT -> do
        mDirs <- resolveComponentDirs plan resolver pkgT
        case mDirs of
          Nothing        -> pure []
          Just (_, dirs) -> do
            mFile <- Locate.findModuleFileIn dirs modT
            case mFile of
              Nothing -> pure []
              Just f  -> Locate.parseExports <$> TIO.readFile f
    }

-- | Resolve a composite component name (e.g. @nike:lib-foo@) into the
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
              roots <- chooseSourceRoots d
              pure (Just (d, roots))
            Nothing -> pure Nothing

-- | Module-name enumeration over an explicit list of source roots.
enumModulesIn :: [FilePath] -> IO [Text]
enumModulesIn roots = do
  paths <- concat <$> mapM
    (\r -> map (drop (length r + 1)) <$> findHs r 4)
    roots
  pure (map (Text.pack . hsToModule) paths)

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

-- | Enumerate every component of a unit (main + sublibs + exes) as
-- @(kind, sourceDirs)@ pairs.  Falls back to a single fallback entry
-- using the heuristic root walk when the unit has no parsed
-- components.
componentsForUnit
  :: BuildPlan -> PackageId -> FilePath
  -> IO [(Comp.ComponentKind, [FilePath])]
componentsForUnit plan pid d =
  case lookupUnit (pkgName pid) plan of
    Just pu | not (null (puLibComponents pu)) ->
      pure
        [ (Comp.ciKind c, Comp.ciHsSourceDirs c)
        | c <- puLibComponents pu
        ]
    _ -> do
      roots <- chooseSourceRoots d
      pure [(Comp.MainLib, roots)]

-- | Pull every cached component index into the in-memory ref.  A unit
-- counts as "fully hydrated" only when /every/ one of its components
-- has cached rows; otherwise it's reported as missing so the
-- background indexer rebuilds the whole set.
hydrateFromCache
  :: BuildPlan
  -> Cache.HyphaPackageCache
  -> [PackageId]
  -> IORef.IORef [Fuzzy.IndexedRow]
  -> IO [PackageId]
hydrateFromCache plan cache pids ref = go [] pids
  where
    go missing [] = pure (reverse missing)
    go missing (pid : rest) = do
      let pkgT  = unPackageName (pkgName pid)
          verT  = unVersion    (pkgVersion pid)
      kinds <- componentKinds plan pid
      case kinds of
        []  -> go (pid : missing) rest
        _   -> do
          let keys = [ componentKey pkgT k | k <- kinds ]
          hits <- mapM (\k -> Cache.haveCachedIndex cache k verT) keys
          if and hits
            then do
              mapM_ (loadKey verT) keys
              go missing rest
            else go (pid : missing) rest

    loadKey verT k = do
      rows <- Cache.readCachedIndex cache k verT
      let indexed =
            [ Fuzzy.mkIndexedRow p m n s | (p, m, n, s) <- rows ]
      indexed `seq`
        IORef.atomicModifyIORef' ref (\old -> (indexed ++ old, ()))

    -- | Just the component kinds for a unit, mirroring the
    -- structure 'componentsForUnit' would emit.  We avoid needing a
    -- source dir here because hydrate works off the cache alone.
    componentKinds :: BuildPlan -> PackageId -> IO [Comp.ComponentKind]
    componentKinds p pid =
      case lookupUnit (pkgName pid) p of
        Just pu | not (null (puLibComponents pu)) ->
          pure [ Comp.ciKind c | c <- puLibComponents pu ]
        _ -> pure [Comp.MainLib]

-- | Walk the source trees of the given packages, extract their module
-- exports, persist the result to the cache, and prepend them to the
-- in-memory ref.  Packages whose source cannot be resolved are silently
-- skipped — the index is a best-effort fallback.
--
-- Per-module rows are built fully /outside/ the atomicModifyIORef'
-- critical section; prepending makes each insert O(|rows|) instead of
-- the O(|index|) behaviour of @old ++ rows@.
buildAndCacheIndex
  :: BuildPlan
  -> Cache.HyphaPackageCache
  -> PackageResolver IO
  -> [PackageId]
  -> IORef.IORef [Fuzzy.IndexedRow]
  -> IORef.IORef Int                    -- ^ packages-done counter
  -> IO ()
buildAndCacheIndex plan cache resolver pids ref doneRef =
  mapM_ indexUnit pids
  where
    -- Local + source-repository-package units land in the project DB;
    -- everything else (store packages) goes to the shared global DB.
    originFor :: PackageId -> CacheOrigin
    originFor pid = case lookupUnit (pkgName pid) plan of
      Just u | puIsLocal u -> OriginProject
      _                    -> OriginGlobal
    -- The done counter bumps once per /unit/, not per component, so
    -- the progress bar continues to read in package units.
    bump = IORef.atomicModifyIORef' doneRef (\n -> (n + 1, ()))

    indexUnit pid = do
      eDir <- resolveSrc resolver pid
      case eDir of
        Left _  -> bump
        Right d -> do
          comps <- componentsForUnit plan pid d
          mapM_ (indexComponent pid) comps
          bump

    indexComponent pid (kind, srcDirs) = do
      let pkgT    = unPackageName (pkgName    pid)
          verT    = unVersion    (pkgVersion pid)
          compKey = componentKey pkgT kind
      mods <- enumModulesIn srcDirs
      rowChunks <- mapM (collectMod compKey srcDirs) mods
      let flatRows = concat rowChunks
          indexed  = [ Fuzzy.mkIndexedRow p m n s
                     | (p, m, n, s) <- flatRows
                     ]
      -- Persist before publishing into memory so a crash mid-stream
      -- never leaves the in-memory view ahead of the cache.
      Cache.writeCachedIndex cache (originFor pid) compKey verT flatRows
      indexed `seq`
        IORef.atomicModifyIORef' ref (\old -> (indexed ++ old, ()))

    -- | Resolve a module file against an explicit list of source roots,
    -- in priority order.
    collectMod compKey srcDirs modPath = do
      mFile <- firstExistingModule srcDirs modPath
      case mFile of
        Nothing -> pure []
        Just f  -> do
          exps <- Locate.parseExports <$> TIO.readFile f
          pure [ (compKey, modPath, e, "")
               | e <- exps
               , not (Text.null e)
               ]

    firstExistingModule [] _ = pure Nothing
    firstExistingModule (r:rs) modPath = do
      let candidate = r FP.</> Text.unpack (Text.replace "." "/" modPath) <> ".hs"
      ok <- Dir.doesFileExist candidate
      if ok then pure (Just candidate) else firstExistingModule rs modPath

-- | Pick the source roots to scan for a package.  If any of the common
-- @hs-source-dirs@ subdirectories exist we walk those exclusively;
-- otherwise we fall back to the package root.  Walking both root /and/
-- the @src/@ subtree double-counts modules and produces duplicate
-- "src.Foo.Bar" / "Foo.Bar" rows in the search index.
chooseSourceRoots :: FilePath -> IO [FilePath]
chooseSourceRoots d = do
  let candidates = [ d FP.</> sub
                   | sub <- ["src", "library", "lib", "Library", "source", "Source"] ]
  existingSubs <- filterExisting candidates
  pure (if null existingSubs then [d] else existingSubs)

filterExisting :: [FilePath] -> IO [FilePath]
filterExisting [] = pure []
filterExisting (p : ps) = do
  ok <- Dir.doesDirectoryExist p
  rest <- filterExisting ps
  pure (if ok then p : rest else rest)

findHs :: FilePath -> Int -> IO [FilePath]
findHs _ depth | depth < 0 = pure []
findHs dir depth = do
  entries <- Dir.listDirectory dir
  let absEntries = map (dir FP.</>) entries
  concat <$> mapM (visit depth) absEntries
  where
    visit d p = do
      isDir <- Dir.doesDirectoryExist p
      if isDir
        then if skipDir (FP.takeFileName p)
               then pure []
               else findHs p (d - 1)
        else if ".hs" `Text.isSuffixOf` Text.pack p
               then pure [p]
               else pure []

    skipDir name = case name of
      '.':_ -> True
      "dist" -> True
      "dist-newstyle" -> True
      "test" -> True
      "tests" -> True
      "bench" -> True
      "benchmarks" -> True
      "Setup" -> True
      _ -> False

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

hsToModule :: FilePath -> String
hsToModule fp =
  let stripped = case Text.stripSuffix ".hs" (Text.pack fp) of
                   Just t  -> Text.unpack t
                   Nothing -> fp
      dotted   = map (\c -> if c == '/' then '.' else c) stripped
  in dotted

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

