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
  , writeIndex
  , readBlob
  , writeBlob
  ) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Monad (void)
import Data.Text (Text)
import Database.SQLite.Simple
  ( Connection, NamedParam ((:=)), Only (..), Query (..), execute, executeMany
  , executeNamed, execute_, open, query_, queryNamed )
import qualified Database.SQLite.Simple as Sql
import System.Directory (XdgDirectory (..), createDirectoryIfMissing, getXdgDirectory)
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
  dir <- getXdgDirectory XdgCache "hypha"
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
  lock <- newMVar ()
  pure (IndexCache conn lock)

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
    \  , sig     TEXT NOT NULL )"
  , "CREATE INDEX IF NOT EXISTS pkg_index_by_pv \
    \  ON pkg_index (pkg, version)"
  , "CREATE TABLE IF NOT EXISTS kv \
    \  ( k TEXT PRIMARY KEY NOT NULL \
    \  , v BLOB NOT NULL )"
  ]

-- | Look up every row whose @name@ matches the given symbol.  When
-- the caller supplies a module qualifier, filter by @mod@ too.  This
-- is the SQL-only primitive; 'Hypha.Search.PackageCache.lookupByName'
-- handles qualified-name parsing and project-shadows-global merging.
lookupRowsByName
  :: IndexCache
  -> Text                                  -- ^ symbol name
  -> Maybe Text                            -- ^ optional module qualifier
  -> IO [(Text, Text, Text, Text)]
lookupRowsByName c name mMod = case mMod of
  Nothing ->
    queryNamed (icConn c)
      "SELECT pkg, mod, name, sig FROM pkg_index WHERE name = :n"
      [":n" := name]
  Just modT ->
    queryNamed (icConn c)
      "SELECT pkg, mod, name, sig FROM pkg_index \
      \WHERE name = :n AND mod = :m"
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
readIndex :: IndexCache -> Text -> Text -> IO [(Text, Text, Text, Text)]
readIndex c pkg ver =
  queryNamed (icConn c)
    "SELECT pkg, mod, name, sig FROM pkg_index \
    \WHERE pkg = :p AND version = :v"
    [":p" := pkg, ":v" := ver]

-- | Replace the cached index for a single @(pkg, version)@.
-- All inserts run inside a single transaction so partial writes never
-- leave a half-populated entry behind.
writeIndex
  :: IndexCache
  -> Text                                 -- ^ package name
  -> Text                                 -- ^ package version
  -> [(Text, Text, Text, Text)]           -- ^ rows: (pkg, mod, name, sig)
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
      let expanded = [ (p, ver, m, n, s) | (p, m, n, s) <- rows ]
      executeMany (icConn c)
        "INSERT INTO pkg_index (pkg, version, mod, name, sig) VALUES (?,?,?,?,?)"
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
