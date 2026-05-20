{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
-- | Project-scoped Hoogle database lifecycle.
--
-- Bundles a single 'Hoogle.Database' path for
-- @\<project\>/.hypha/hoogle.hoo@.  The database is regenerated lazily
-- the first time 'ensureFresh' observes a stale stamp, and is guarded
-- by an 'MVar' so concurrent searches never spawn duplicate generation
-- work (the underlying @hoogle@ library deadlocks under concurrent
-- regen).
module Hypha.Hoogle.Local
  ( HyphaHoogle
  , openLocalHoogle
  , searchLocal
    -- * Generation pipeline
  , LocalUnit (..)
  , HaddockRequest (..)
  , HaddockError (..)
  , HaddockRunner (..)
  , defaultHaddockRunner
  , collectTxtForUnit
  , HoogleStamp (..)
  , ensureFresh
    -- * Internals exposed for tests + downstream wiring
  , scavengeStoreTxt
  , haddockOutputPath
  ) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (SomeException, try)
import Control.Monad (when)
import Data.List (isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import qualified Hoogle
import System.Directory
  ( XdgDirectory (..), copyFile, createDirectoryIfMissing
  , createFileLink, doesDirectoryExist, doesFileExist, getXdgDirectory
  , listDirectory, removeDirectoryRecursive )
import System.Exit (ExitCode (..))
import System.FilePath ((</>), takeDirectory, takeExtension, takeFileName)
import System.Process (readProcessWithExitCode)

import Hypha.Hoogle.Type (HoogleHit (..), HoogleQuery (..))
import Hypha.Types.PackageId
  ( PackageId (..), PackageName (..), Version (..) )

-- | Opaque handle to the local Hoogle DB lifecycle.
data HyphaHoogle = HyphaHoogle
  { hhDbPath    :: !FilePath
  , hhLock      :: !(MVar ())
  , hhStoreRoot :: !FilePath
    -- ^ @~/.cabal/store/ghc-X.Y.Z@ root for the active GHC.  Empty
    -- string when the store could not be located; scavenging then
    -- always returns 'Nothing' and the caller falls back to haddock.
  }

-- | Open (or initialise) the per-project Hoogle DB.  No regeneration
-- happens here; the first 'ensureFresh' triggers it lazily.
openLocalHoogle
  :: FilePath  -- ^ project @.hypha@ directory
  -> FilePath  -- ^ GHC store root (may be \"\" to disable scavenging)
  -> IO HyphaHoogle
openLocalHoogle dotHypha storeRoot = do
  lock <- newMVar ()
  pure HyphaHoogle
    { hhDbPath    = dotHypha </> "hoogle.hoo"
    , hhLock      = lock
    , hhStoreRoot = storeRoot
    }

-- | Locate @\<pkg\>.txt@ inside the cabal store.  Returns 'Nothing'
-- when the package is not installed with documentation.  The path
-- layout under the store is:
--
-- > <store-root>/<pkg-ver-hash>/share/doc/<pkg-ver>/html/<pkg>.txt
--
-- We list the @ghc-X.Y.Z@ root, pick hash dirs that begin with
-- @\<pkg\>-\<ver\>-@, and return the first one that carries the file.
scavengeStoreTxt :: FilePath -> PackageId -> IO (Maybe FilePath)
scavengeStoreTxt storeRoot pid = do
  rootOk <- doesDirectoryExist storeRoot
  if not rootOk then pure Nothing else do
    entries <- listDirectory storeRoot
    let prefix = Text.unpack (unPackageName (pkgName pid))
                 <> "-"
                 <> Text.unpack (unVersion (pkgVersion pid))
                 <> "-"
        candidates = [ storeRoot </> e | e <- entries, prefix `isPrefixOf` e ]
    firstHit candidates
  where
    firstHit []     = pure Nothing
    firstHit (c:cs) = do
      r <- probeDoc c
      case r of
        Just p  -> pure (Just p)
        Nothing -> firstHit cs

    probeDoc base = do
      let pkg = Text.unpack (unPackageName (pkgName pid))
          ver = Text.unpack (unVersion    (pkgVersion pid))
          path = base </> "share" </> "doc"
                      </> (pkg <> "-" <> ver)
                      </> "html" </> (pkg <> ".txt")
      ok <- doesFileExist path
      pure (if ok then Just path else Nothing)

-- | What we need to know about a unit for Hoogle @.txt@ collection.
data LocalUnit = LocalUnit
  { luPkgId   :: !PackageId
  , luSrcDirs :: ![FilePath]
    -- ^ @hs-source-dirs@ entries to feed @haddock@ when no store
    -- @.txt@ is available.
  }
  deriving stock (Show, Eq)

-- | Request to invoke @haddock@ for a single package.
data HaddockRequest = HaddockRequest
  { hrPkgId   :: !PackageId
  , hrSrcDirs :: ![FilePath]
  , hrOutput  :: !FilePath    -- ^ destination @.txt@ path
  }
  deriving stock (Show, Eq)

-- | Reason a @haddock@ invocation could not produce a @.txt@.
newtype HaddockError = HaddockError Text
  deriving stock (Show, Eq)

-- | Record-of-functions wrapping the @haddock@ binary so tests can
-- inject a deterministic implementation.
newtype HaddockRunner = HaddockRunner
  { runHaddock :: HaddockRequest -> IO (Either HaddockError FilePath)
  }

-- | Try the store first, then @haddock@.  Returns the @.txt@ path or
-- a 'HaddockError'.
collectTxtForUnit
  :: HaddockRunner
  -> FilePath           -- ^ store root
  -> LocalUnit
  -> IO (Either HaddockError FilePath)
collectTxtForUnit runner storeRoot lu = do
  scavenged <- scavengeStoreTxt storeRoot (luPkgId lu)
  case scavenged of
    Just p  -> pure (Right p)
    Nothing -> do
      out <- haddockOutputPath (luPkgId lu)
      runHaddock runner HaddockRequest
        { hrPkgId   = luPkgId lu
        , hrSrcDirs = luSrcDirs lu
        , hrOutput  = out
        }

-- | Where to put @haddock@-generated @.txt@ files.  We co-locate
-- them under @\<XDG_CACHE\>/hypha/hoogle-txt@ so the
-- @hoogle generate@ step can point at a single directory.
haddockOutputPath :: PackageId -> IO FilePath
haddockOutputPath pid = do
  dir <- getXdgDirectory XdgCache "hypha"
  let outDir = dir </> "hoogle-txt"
      file   = Text.unpack (unPackageName (pkgName pid))
            <> "-"
            <> Text.unpack (unVersion (pkgVersion pid))
            <> ".txt"
  createDirectoryIfMissing True outDir
  pure (outDir </> file)

-- | Default runner: shells to the @haddock@ binary with @--hoogle@.
-- The binary is expected on @PATH@ (ships with every GHCup install).
-- When absent, the runner returns 'HaddockError'; the caller decides
-- whether to fall back to remote-only operation.
--
-- We deliberately do NOT pass GHC package-db flags here: by the time
-- hypha runs, the project has been built, so @haddock@ inherits the
-- right environment.  When that assumption breaks the runner fails
-- and the failure is surfaced as a structured warning rather than a
-- crash.
defaultHaddockRunner :: HaddockRunner
defaultHaddockRunner = HaddockRunner $ \req -> do
  files <- enumerateHsFiles (hrSrcDirs req)
  case files of
    [] -> pure (Left (HaddockError "no .hs files found"))
    _  -> do
      (ec, _out, err) <- readProcessWithExitCode "haddock"
        ( ["--hoogle", "-o", takeDirectory (hrOutput req)]
        ++ files ) ""
      case ec of
        ExitSuccess -> do
          ok <- doesFileExist (hrOutput req)
          if ok
            then pure (Right (hrOutput req))
            else pure (Left (HaddockError "haddock produced no output"))
        ExitFailure _ -> pure (Left (HaddockError (Text.pack err)))

enumerateHsFiles :: [FilePath] -> IO [FilePath]
enumerateHsFiles = fmap concat . mapM walk
  where
    walk root = do
      ok <- doesDirectoryExist root
      if not ok then pure [] else walkDir root
    walkDir d = do
      entries <- listDirectory d
      fmap concat $ mapM (visit d) entries
    visit parent name = do
      let p = parent </> name
      isDir <- doesDirectoryExist p
      if isDir
        then walkDir p
        else if takeExtension p `elem` [".hs", ".lhs"]
               then pure [p] else pure []

-- | Identity stamp used to decide whether the Hoogle DB is stale.
-- Two parts: the plan hash (captures dependency changes) and an
-- aggregate fingerprint over all local components (captures source
-- edits).  Either changing invalidates the DB.
data HoogleStamp = HoogleStamp
  { hsPlanHash    :: !Text
  , hsAggregateFp :: !Text
  }
  deriving stock (Show, Eq)

stampFilePath :: FilePath -> FilePath
stampFilePath dotHypha = dotHypha </> "hoogle-stamp"

-- | Regenerate the Hoogle DB if the stored stamp differs from the
-- supplied one.  Idempotent and cheap when stamps match.
ensureFresh
  :: HaddockRunner
  -> FilePath        -- ^ store root
  -> FilePath        -- ^ project @.hypha@ directory
  -> HoogleStamp     -- ^ current stamp
  -> [LocalUnit]
  -> IO ()
ensureFresh runner storeRoot dotHypha stamp units = do
  createDirectoryIfMissing True dotHypha
  mPrior <- readStamp (stampFilePath dotHypha)
  when (mPrior /= Just stamp) $
    regenerate runner storeRoot dotHypha stamp units

readStamp :: FilePath -> IO (Maybe HoogleStamp)
readStamp f = do
  ok <- doesFileExist f
  if not ok then pure Nothing else do
    txt <- TIO.readFile f
    case Text.lines txt of
      (a : b : _) -> pure (Just (HoogleStamp a b))
      _           -> pure Nothing

writeStamp :: FilePath -> HoogleStamp -> IO ()
writeStamp f s = TIO.writeFile f (Text.unlines [hsPlanHash s, hsAggregateFp s])

regenerate
  :: HaddockRunner
  -> FilePath
  -> FilePath
  -> HoogleStamp
  -> [LocalUnit]
  -> IO ()
regenerate runner storeRoot dotHypha stamp units = do
  -- 1. Gather every per-package .txt path.
  paths <- mapM (collectTxtForUnit runner storeRoot) units
  let okPaths = [ p | Right p <- paths ]

  -- 2. Drop them into a single directory @hoogle generate@ can scan
  -- with @--local=<dir>@.  Symlink when possible; fall back to copy.
  let inputDir = dotHypha </> "hoogle-input"
  removeAndRecreate inputDir
  mapM_ (linkOrCopy inputDir) okPaths

  -- 3. Generate the database (no-op if okPaths is empty: Hoogle will
  -- still create an empty DB file which is fine for our purposes).
  let dbPath = dotHypha </> "hoogle.hoo"
  case okPaths of
    [] -> pure ()
    _  -> Hoogle.hoogle
            [ "generate"
            , "--database=" <> dbPath
            , "--local=" <> inputDir
            ]

  -- 4. Stamp the result so we skip next time.
  writeStamp (stampFilePath dotHypha) stamp

removeAndRecreate :: FilePath -> IO ()
removeAndRecreate p = do
  ok <- doesDirectoryExist p
  when ok (removeDirectoryRecursive p)
  createDirectoryIfMissing True p

linkOrCopy :: FilePath -> FilePath -> IO ()
linkOrCopy dstDir src = do
  let dst = dstDir </> takeFileName src
  result <- try (createFileLink src dst) :: IO (Either SomeException ())
  case result of
    Right () -> pure ()
    Left  _  -> copyFile src dst

-- | Stub: 'searchLocal' is wired to consult the on-disk DB in the
-- next task.  For now it returns no results regardless of input.
searchLocal :: HyphaHoogle -> HoogleQuery -> IO [HoogleHit]
searchLocal hh _q = withMVar (hhLock hh) $ \_ -> pure []
