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
import qualified Hypha.Search.Cache as Cache
import qualified Hypha.Search.Fuzzy as Fuzzy
import qualified Hypha.Server.App as App
import qualified Hypha.Server.Haddock.Rewrite as Rewrite
import qualified Hypha.Server.Slots as Slots
import qualified Hypha.Source.Extract as Extract
import qualified Hypha.Source.Locate as Locate
import Hypha.Types.BuildPlan (BuildPlan (..), PlannedUnit (..))
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
  :: BuildPlan
  -> BuildEnv IO
  -> HackageClient IO
  -> PackageResolver IO
  -> Hoogle IO
  -> ServerOpts
  -> IO (Either BindError ())
runServer plan env hclient resolver hoogle opts = do
  cfg <- buildServerConfig plan env hclient resolver hoogle
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
  :: BuildPlan
  -> BuildEnv IO
  -> HackageClient IO
  -> PackageResolver IO
  -> Hoogle IO
  -> IO App.ServerConfig
buildServerConfig plan _env _hclient resolver _hoogle = do
  let pids      = planPackageIds plan
      packages  = map (unPackageName . pkgName) pids
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
  cache       <- Cache.defaultCachePath >>= Cache.openIndexCache
  -- Hydrate from the on-disk cache synchronously before the server
  -- accepts requests so the common (warm) path renders results
  -- immediately on the first keystroke.  Anything not yet cached gets
  -- built in the background and persisted for next time.
  missing  <- hydrateFromCache cache pids indexRef
  IORef.writeIORef totalRef (length missing)
  case missing of
    [] -> IORef.writeIORef readyRef True
    _  -> do
      _ <- forkIO $ do
        r <- try (buildAndCacheIndex cache resolver missing indexRef doneRef)
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
        ePid <- resolvePkg resolver (PackageName pkgT)
        case ePid of
          Left _ -> pure Nothing
          Right rp -> do
            eDir <- resolveSrc resolver (rpPkgId rp)
            case eDir of
              Left _  -> pure Nothing
              Right d -> do
                mFile <- Locate.findModuleFile d modT
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
                    mLoc <- Locate.locateSymbolDefinitionInDir d modT symT
                    (info, resolvedMod, lineOverride) <-
                      case (Extract.siSignature info0, mLoc) of
                        (Nothing, Just loc) | Locate.slPath loc /= f -> do
                          src' <- TIO.readFile (Locate.slPath loc)
                          let info' = Extract.extractSymbolInfo src' symT
                              modT' = modulePathFromFile d (Locate.slPath loc)
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
        ePid <- resolvePkg resolver (PackageName pkgT)
        case ePid of
          Left _ -> pure Nothing
          Right rp -> do
            let pid = rpPkgId rp
            eDir <- resolveSrc resolver pid
            case eDir of
              Left _   -> pure Nothing
              Right d  -> do
                mFile <- Locate.findModuleFile d modT
                case mFile of
                  Nothing -> pure Nothing
                  Just f  -> Just <$> TIO.readFile f
    , App.scPackageInfo  = \pkgT -> do
        ePid <- resolvePkg resolver (PackageName pkgT)
        case ePid of
          Left _   -> pure Nothing
          Right rp -> do
            let pid = rpPkgId rp
                ver = unVersion (pkgVersion pid)
            mods <- listModulesFor resolver pid
            pure (Just (ver, mods))
    , App.scModuleExports = \pkgT modT -> do
        ePid <- resolvePkg resolver (PackageName pkgT)
        case ePid of
          Left _   -> pure []
          Right rp -> do
            eDir <- resolveSrc resolver (rpPkgId rp)
            case eDir of
              Left _  -> pure []
              Right d -> do
                mFile <- Locate.findModuleFile d modT
                case mFile of
                  Nothing -> pure []
                  Just f  -> Locate.parseExports <$> TIO.readFile f
    }

-- | Pull every cached package index into the in-memory ref.  Returns the
-- 'PackageId's that had no cached entry yet, so the caller can build
-- them in the background.
hydrateFromCache
  :: Cache.IndexCache
  -> [PackageId]
  -> IORef.IORef [Fuzzy.IndexedRow]
  -> IO [PackageId]
hydrateFromCache cache pids ref = go [] pids
  where
    go missing [] = pure (reverse missing)
    go missing (pid : rest) = do
      let pkgT = unPackageName (pkgName pid)
          verT = unVersion    (pkgVersion pid)
      hit <- Cache.haveIndex cache pkgT verT
      if hit
        then do
          rows <- Cache.readIndex cache pkgT verT
          let indexed =
                [ Fuzzy.mkIndexedRow p m n s
                | (p, m, n, s) <- rows
                ]
          indexed `seq`
            IORef.atomicModifyIORef' ref (\old -> (indexed ++ old, ()))
          go missing rest
        else go (pid : missing) rest

-- | Walk the source trees of the given packages, extract their module
-- exports, persist the result to the cache, and prepend them to the
-- in-memory ref.  Packages whose source cannot be resolved are silently
-- skipped — the index is a best-effort fallback.
--
-- Per-module rows are built fully /outside/ the atomicModifyIORef'
-- critical section; prepending makes each insert O(|rows|) instead of
-- the O(|index|) behaviour of @old ++ rows@.
buildAndCacheIndex
  :: Cache.IndexCache
  -> PackageResolver IO
  -> [PackageId]
  -> IORef.IORef [Fuzzy.IndexedRow]
  -> IORef.IORef Int                    -- ^ packages-done counter
  -> IO ()
buildAndCacheIndex cache resolver pids ref doneRef = mapM_ indexPkg pids
  where
    bump = IORef.atomicModifyIORef' doneRef (\n -> (n + 1, ()))

    indexPkg pid = do
      eDir <- resolveSrc resolver pid
      case eDir of
        Left _  -> bump
        Right d -> do
          mods <- enumModules d
          rowChunks <- mapM (collectMod pid d) mods
          let pkgT     = unPackageName (pkgName    pid)
              verT     = unVersion    (pkgVersion pid)
              flatRows = concat rowChunks
              indexed  = [ Fuzzy.mkIndexedRow p m n s
                         | (p, m, n, s) <- flatRows
                         ]
          -- Persist before publishing into memory so a crash mid-stream
          -- never leaves the in-memory view ahead of the cache.
          Cache.writeIndex cache pkgT verT flatRows
          indexed `seq`
            IORef.atomicModifyIORef' ref (\old -> (indexed ++ old, ()))
          bump

    collectMod pid d modPath = do
      mFile <- Locate.findModuleFile d modPath
      case mFile of
        Nothing -> pure []
        Just f  -> do
          exps <- Locate.parseExports <$> TIO.readFile f
          let pkgT = unPackageName (pkgName pid)
          pure [ (pkgT, modPath, e, "")
               | e <- exps
               , not (Text.null e)
               ]

    enumModules d = do
      roots <- chooseSourceRoots d
      paths <- concat <$> mapM (\r -> map (drop (length r + 1)) <$> findHs r 4) roots
      pure (map (Text.pack . hsToModule) paths)

-- | Walk the resolved source tree and list every @.hs@ file as a dotted
-- module path.  Skips the standard build/test/bench directories so the
-- module index reflects the library's exposed-modules-shape closely enough.
listModulesFor :: PackageResolver IO -> PackageId -> IO [Text]
listModulesFor resolver pid = do
  eDir <- resolveSrc resolver pid
  case eDir of
    Left _   -> pure []
    Right d  -> do
      roots <- chooseSourceRoots d
      paths <- concat <$> mapM (\r -> map (drop (length r + 1)) <$> findHs r 4) roots
      pure (map (Text.pack . hsToModule) paths)

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

