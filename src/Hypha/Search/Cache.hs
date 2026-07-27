{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
-- | Persistent, on-disk cache for the live search index.
--
-- The first @hypha server@ run on a project pays the cost of walking
-- every package source tree and parsing its export lists; subsequent
-- runs read the result back out of SQLite in milliseconds.
--
-- The schema is intentionally generic and keyed on @(package, version)@
-- so the same DB is reused across every project on the host: if two
-- projects depend on @containers-0.6.7@, the second one inherits the
-- first one's work.  The DB is co-located with our other caches under
-- @$XDG_CACHE_HOME/hypha/hypha.db@.
--
-- The module also exposes a small key-value table ('readBlob' /
-- 'writeBlob') so other features can piggyback on the same file
-- without having to provision their own database.
module Hypha.Search.Cache
  ( IndexCache
  , openIndexCache
  , defaultCachePath
  , haveIndex
  , readIndex
  , lookupRowsByName
  , readFingerprint
  , writeFingerprint
  , writeIndex
  , readBlob
  , writeBlob
  ) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Monad (void)
import Database.SQLite.Simple
import Database.SQLite.Simple qualified as Sql
import Data.Text (Text)
import Data.Text qualified as Text
import System.IO (hPutStrLn, stderr)

import Hypha.Cache qualified as Cache
import Hypha.Search.Index
  ( IndexRow (..), Visibility (Internal), currentIndexFormat
  , visibilityFromText, visibilityToText )
import Hypha.Types.ComponentName (ComponentKey (..))
import Hypha.Types.SymbolPath (ModulePath (..), Signature (..), SymbolName (..))
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

-- | Handle on the on-disk cache.  Wraps a single 'Connection' guarded by
-- an 'MVar' so concurrent writers serialise cleanly (SQLite's own write
-- lock would do it too, but the MVar makes the contention visible in
-- one place and avoids @SQLITE_BUSY@ surprises under heavy fan-out).
data IndexCache = IndexCache
  { icConn :: !Connection
  , icLock :: !(MVar ())
  }

-- | Default cache location: @$XDG_CACHE_HOME/hypha/hypha.db@.
defaultCachePath :: IO FilePath
defaultCachePath = do
  dir <- Cache.cacheRoot
  createDirectoryIfMissing True dir
  pure (dir </> "hypha.db")

-- | Open the cache at the given path, creating tables if necessary.
openIndexCache :: FilePath -> IO IndexCache
openIndexCache path = do
  conn <- open path
  -- WAL gives readers concurrent access while the indexer is still
  -- writing, which matches our background-build pattern.
  execute_ conn "PRAGMA journal_mode = WAL"
  execute_ conn "PRAGMA synchronous = NORMAL"
  mapM_ (execute_ conn) schema
  migrateAddColumn conn "pkg_index_meta" "fingerprint" "TEXT"
  -- Columns first, so opening a generation-1 database does not throw
  -- before 'ensureIndexFormat' gets the chance to clear it.
  migrateAddColumn conn "pkg_index" "def_mod"    "TEXT NOT NULL DEFAULT ''"
  migrateAddColumn conn "pkg_index" "visibility" "TEXT NOT NULL DEFAULT ''"
  lock <- newMVar ()
  let c = IndexCache conn lock
  ensureIndexFormat c
  pure c

-- | Discard rows written under an older row format.
--
-- No migration is attempted.  Generation-1 rows may carry a module name
-- derived from a file path or a signature resolved by symbol name, and
-- neither is detectable from the row itself — so the choice is re-index or
-- lie, and a search index that lies is worse than one that is briefly
-- empty.  The rebuild is a background pass the server already reports on.
ensureIndexFormat :: IndexCache -> IO ()
ensureIndexFormat c = do
  stored <- readBlob c indexFormatKey
  let current = Text.pack (show currentIndexFormat)
  if stored == Just current
    then pure ()
    else do
      withWrite c $ Sql.withTransaction (icConn c) $ do
        execute_ (icConn c) "DELETE FROM pkg_index"
        execute_ (icConn c) "DELETE FROM pkg_index_meta"
      writeBlob c indexFormatKey current

-- | @kv@ key holding the row-format generation of a database.
indexFormatKey :: Text
indexFormatKey = "index_format"

schema :: [Query]
schema =
  [ "CREATE TABLE IF NOT EXISTS pkg_index_meta \
    \  ( pkg     TEXT NOT NULL \
    \  , version TEXT NOT NULL \
    \  , indexed_at INTEGER NOT NULL \
    \  , fingerprint TEXT \
    \  , PRIMARY KEY (pkg, version) )"
  , "CREATE TABLE IF NOT EXISTS pkg_index \
    \  ( pkg     TEXT NOT NULL \
    \  , version TEXT NOT NULL \
    \  , mod     TEXT NOT NULL \
    \  , name    TEXT NOT NULL \
    \  , sig     TEXT NOT NULL \
    \  , def_mod TEXT NOT NULL \
    \  , visibility TEXT NOT NULL )"
  , "CREATE INDEX IF NOT EXISTS pkg_index_by_pv \
    \  ON pkg_index (pkg, version)"
  , "CREATE TABLE IF NOT EXISTS kv \
    \  ( k TEXT PRIMARY KEY NOT NULL \
    \  , v BLOB NOT NULL )"
  ]

-- | Read the stored fingerprint for a @(pkg, version)@ pair.
readFingerprint :: IndexCache -> Text -> Text -> IO (Maybe Text)
readFingerprint c pkg ver = do
  rs <- queryNamed (icConn c)
          "SELECT fingerprint FROM pkg_index_meta \
          \WHERE pkg = :p AND version = :v LIMIT 1"
          [":p" := pkg, ":v" := ver]
          :: IO [Only (Maybe Text)]
  pure (case rs of
          (Only mfp : _) -> mfp
          _              -> Nothing)

-- | Stamp the fingerprint for an existing @pkg_index_meta@ row,
-- inserting a placeholder row with @indexed_at = 0@ when none yet
-- exists (the real value is written by 'writeIndex').
writeFingerprint :: IndexCache -> Text -> Text -> Text -> IO ()
writeFingerprint c pkg ver fp = withWrite c $ executeNamed (icConn c)
  "INSERT INTO pkg_index_meta (pkg, version, indexed_at, fingerprint) \
  \VALUES (:p, :v, 0, :f) \
  \ON CONFLICT(pkg, version) DO UPDATE SET fingerprint = :f"
  [":p" := pkg, ":v" := ver, ":f" := fp]

-- | Look up every row whose @name@ matches the given symbol.  When
-- the caller supplies a module qualifier, filter by @mod@ too.  This
-- is the SQL-only primitive; 'Hypha.Search.PackageCache.lookupByName'
-- handles qualified-name parsing and project-shadows-global merging.
lookupRowsByName
  :: IndexCache
  -> Text                                  -- ^ symbol name
  -> Maybe Text                            -- ^ optional module qualifier
  -> IO [IndexRow]
lookupRowsByName c name mMod = (reportAnomalies . map fromStored =<<) $ case mMod of
  Nothing ->
    queryNamed (icConn c)
      (Query ("SELECT " <> rowColumns <> " FROM pkg_index WHERE name = :n"))
      [":n" := name]
  Just modT ->
    queryNamed (icConn c)
      (Query ("SELECT " <> rowColumns <> " FROM pkg_index \
              \WHERE name = :n AND mod = :m"))
      [":n" := name, ":m" := modT]

withWrite :: IndexCache -> IO a -> IO a
withWrite c io = withMVar (icLock c) (\_ -> io)

-- | Add a column to an existing table if it is not already present.
-- SQLite has no @ADD COLUMN IF NOT EXISTS@, so we probe
-- @PRAGMA table_info@ first.  Used to migrate caches created before
-- the fingerprint column existed.
migrateAddColumn :: Connection -> Text -> Text -> Text -> IO ()
migrateAddColumn conn table column colType = do
  cols <- query_ conn
            (Query ("PRAGMA table_info(" <> table <> ")"))
            :: IO [(Int, Text, Text, Int, Maybe Text, Int)]
  let names = [n | (_, n, _, _, _, _) <- cols]
  if column `elem` names
    then pure ()
    else execute_ conn
           (Query
             ("ALTER TABLE " <> table
              <> " ADD COLUMN " <> column
              <> " " <> colType))

-- | Is there already a cached index for this @(pkg, version)@?
haveIndex :: IndexCache -> Text -> Text -> IO Bool
haveIndex c pkg ver = do
  rs <- queryNamed (icConn c)
          "SELECT 1 FROM pkg_index_meta WHERE pkg = :p AND version = :v LIMIT 1"
          [":p" := pkg, ":v" := ver] :: IO [Only Int]
  pure (not (null rs))

-- | Read the cached @(pkg, mod, name, sig)@ rows for a given package
-- version.  Returns @[]@ when no entry exists.
readIndex :: IndexCache -> Text -> Text -> IO [IndexRow]
readIndex c pkg ver =
  (reportAnomalies . map fromStored =<<) $ queryNamed (icConn c)
    (Query ("SELECT " <> rowColumns <> " FROM pkg_index \
            \WHERE pkg = :p AND version = :v"))
    [":p" := pkg, ":v" := ver]

-- | The column list, written once so the SELECTs and 'fromStored' cannot
-- drift apart.  Column order is a wire format: it is spelled out rather
-- than derived.
rowColumns :: Text
rowColumns = "pkg, mod, name, sig, def_mod, visibility"

-- | Rebuild a row from its stored columns, along with a description of
-- anything about it we could not make sense of.
--
-- An unrecognised visibility can only come from a row
-- 'ensureIndexFormat' should already have cleared, so the anomaly travels
-- out to 'reportAnomalies' — which is in 'IO' and can actually say
-- something — rather than being silently defaulted here.  The row is kept
-- as 'Internal', the ranking-neutral choice.
fromStored :: (Text, Text, Text, Text, Text, Text) -> (IndexRow, Maybe Text)
fromStored (pkg, modPath, name, sig, defMod, vis) =
  ( IndexRow
      { rowComponent  = ComponentKey pkg
      , rowModule     = ModulePath modPath
      , rowName       = SymbolName name
      , rowSignature  = Signature sig
      , rowDefModule  = ModulePath defMod
      , rowVisibility = maybe Internal id (visibilityFromText vis)
      }
  , case visibilityFromText vis of
      Just _  -> Nothing
      Nothing -> Just
        (pkg <> "/" <> modPath <> ": unrecognised visibility " <> Text.pack (show vis))
  )

-- | Trace every anomaly 'fromStored' found, then hand back the rows.
reportAnomalies :: [(IndexRow, Maybe Text)] -> IO [IndexRow]
reportAnomalies rows = do
  mapM_ report [ a | (_, Just a) <- rows ]
  pure (map fst rows)
  where
    report a = hPutStrLn stderr ("hypha index cache: " <> Text.unpack a)

-- | Replace the cached index for a single @(pkg, version)@.
-- All inserts run inside a single transaction so partial writes never
-- leave a half-populated entry behind.
writeIndex
  :: IndexCache
  -> Text                                 -- ^ package name
  -> Text                                 -- ^ package version
  -> [IndexRow]
  -> IO ()
writeIndex c pkg ver rows = withWrite c $ Sql.withTransaction (icConn c) $ do
  executeNamed (icConn c)
    "DELETE FROM pkg_index WHERE pkg = :p AND version = :v"
    [":p" := pkg, ":v" := ver]
  executeNamed (icConn c)
    "DELETE FROM pkg_index_meta WHERE pkg = :p AND version = :v"
    [":p" := pkg, ":v" := ver]
  case rows of
    [] -> pure ()
    _  -> do
      let expanded =
            [ ( unComponentKey (rowComponent r)
              , ver
              , unModulePath (rowModule r)
              , unSymbolName (rowName r)
              , unSignature (rowSignature r)
              , unModulePath (rowDefModule r)
              , visibilityToText (rowVisibility r)
              )
            | r <- rows
            ]
      executeMany (icConn c)
        "INSERT INTO pkg_index \
        \  (pkg, version, mod, name, sig, def_mod, visibility) \
        \VALUES (?,?,?,?,?,?,?)"
        expanded
  -- @strftime('%s','now')@ stores the timestamp as a Unix second so the
  -- meta row stays human-inspectable from a sqlite3 prompt.
  void $ execute (icConn c)
    "INSERT INTO pkg_index_meta (pkg, version, indexed_at) \
    \VALUES (?, ?, CAST(strftime('%s','now') AS INTEGER))"
    (pkg, ver)

-- | Read a value from the generic key-value table.
readBlob :: IndexCache -> Text -> IO (Maybe Text)
readBlob c k = do
  rs <- queryNamed (icConn c) "SELECT v FROM kv WHERE k = :k LIMIT 1"
          [":k" := k] :: IO [Only Text]
  pure (case rs of (Only v : _) -> Just v; [] -> Nothing)

-- | Write or overwrite a value in the key-value table.
writeBlob :: IndexCache -> Text -> Text -> IO ()
writeBlob c k v = withWrite c $
  execute (icConn c)
    "INSERT INTO kv (k, v) VALUES (?,?) \
    \ON CONFLICT(k) DO UPDATE SET v = excluded.v"
    (k, v)
