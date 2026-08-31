{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
-- | Persistent, on-disk cache for the live search index.
--
-- The first @hypha server@ run on a project pays the cost of walking
-- every package source tree and parsing its export lists; subsequent
-- runs read the result back out of SQLite in milliseconds.
--
-- The schema is keyed on @(component, version, unit-id)@ so the same DB
-- is reused across every project on the host: if two projects depend on
-- @containers-0.6.7@ /in the same configuration/, the second one
-- inherits the first one's work.  The unit-id is what makes that "same
-- configuration" precise — see 'UnitPin'.
--
-- The DB is co-located with our other caches under
-- @$XDG_CACHE_HOME/hypha/hypha.db@.
--
-- The module also exposes a small key-value table ('readBlob' /
-- 'writeBlob') so other features can piggyback on the same file
-- without having to provision their own database.
module Hypha.Search.Cache
  ( IndexCache
  , CacheScope (..)
  , UnitPin (..)
  , scopeForPlan
  , VersionedRow (..)
  , openIndexCache
  , defaultCachePath
  , readIndex
  , lookupRowsByName
  , lookupRowsInModule
  , readFingerprint
  , writeFingerprint
  , writeIndex
  , readBlob
  , writeBlob
  ) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Monad (void)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Database.SQLite.Simple
import Database.SQLite.Simple qualified as Sql
import Data.Text (Text)
import Data.Text qualified as Text
import System.IO (hPutStrLn, stderr)

import Hypha.Cache qualified as Cache
import Hypha.Search.Index
  ( DefinitionRef (..), IndexRow (..), Visibility (Internal)
  , currentIndexFormat, visibilityFromText, visibilityToText )
import Hypha.Types.ComponentName
  ( ComponentKey (..), ComponentName (..), parseComponentName )
import Hypha.Types.PackageId (PackageName, UnitId (..), Version (..))
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
  mapM_ (execute_ conn) kvSchema
  lock <- newMVar ()
  let c = IndexCache conn lock
  -- The guard runs between the two halves of the schema: it may drop a
  -- previous generation's index tables, and what replaces them has to be
  -- created afterwards.
  ensureIndexFormat c
  mapM_ (execute_ conn) indexSchema
  -- Same-generation databases that predate a column still need it added.
  migrateAddColumn conn "pkg_index_meta" "fingerprint" "TEXT"
  migrateAddColumn conn "pkg_index" "def_mod"    "TEXT NOT NULL DEFAULT ''"
  migrateAddColumn conn "pkg_index" "def_pkg"    "TEXT NOT NULL DEFAULT ''"
  migrateAddColumn conn "pkg_index" "visibility" "TEXT NOT NULL DEFAULT ''"
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
      -- Dropped rather than emptied: generation 6 moved the primary key
      -- itself (it gained @unit_id@), and SQLite cannot alter one in
      -- place.  The rows were going anyway, so the table goes with them.
      withWrite c $ Sql.withTransaction (icConn c) $ do
        execute_ (icConn c) "DROP TABLE IF EXISTS pkg_index"
        execute_ (icConn c) "DROP TABLE IF EXISTS pkg_index_meta"
      writeBlob c indexFormatKey current

-- | @kv@ key holding the row-format generation of a database.
indexFormatKey :: Text
indexFormatKey = "index_format"

-- | The key-value table, created before anything else: the format guard
-- itself is stored in it.
kvSchema :: [Query]
kvSchema =
  [ "CREATE TABLE IF NOT EXISTS kv \
    \  ( k TEXT PRIMARY KEY NOT NULL \
    \  , v BLOB NOT NULL )"
  ]

-- | The index tables.  Created /after/ the format guard has had its
-- chance to drop a previous generation's, because a stale table cannot
-- be extended into the current shape — generation 6 moved the primary
-- key.
indexSchema :: [Query]
indexSchema =
  [ "CREATE TABLE IF NOT EXISTS pkg_index_meta \
    \  ( pkg     TEXT NOT NULL \
    \  , version TEXT NOT NULL \
    \  , unit_id TEXT NOT NULL \
    \  , indexed_at INTEGER NOT NULL \
    \  , fingerprint TEXT \
    \  , PRIMARY KEY (pkg, version, unit_id) )"
  , "CREATE TABLE IF NOT EXISTS pkg_index \
    \  ( pkg     TEXT NOT NULL \
    \  , version TEXT NOT NULL \
    \  , unit_id TEXT NOT NULL \
    \  , mod     TEXT NOT NULL \
    \  , name    TEXT NOT NULL \
    \  , sig     TEXT NOT NULL \
    \  , def_mod TEXT NOT NULL \
    \  , def_pkg TEXT NOT NULL \
    \  , visibility TEXT NOT NULL )"
  , "CREATE INDEX IF NOT EXISTS pkg_index_by_pvu \
    \  ON pkg_index (pkg, version, unit_id)"
  ]

-- | Read the stored fingerprint for a @(pkg, version, unit-id)@ entry.
readFingerprint :: IndexCache -> Text -> Text -> UnitId -> IO (Maybe Text)
readFingerprint c pkg ver unit = do
  rs <- queryNamed (icConn c)
          "SELECT fingerprint FROM pkg_index_meta \
          \WHERE pkg = :p AND version = :v AND unit_id = :u LIMIT 1"
          [":p" := pkg, ":v" := ver, ":u" := unUnitId unit]
          :: IO [Only (Maybe Text)]
  pure (case rs of
          (Only mfp : _) -> mfp
          _              -> Nothing)

-- | Stamp the fingerprint for an existing @pkg_index_meta@ row,
-- inserting a placeholder row with @indexed_at = 0@ when none yet
-- exists (the real value is written by 'writeIndex').
writeFingerprint :: IndexCache -> Text -> Text -> UnitId -> Text -> IO ()
writeFingerprint c pkg ver unit fp = withWrite c $ executeNamed (icConn c)
  "INSERT INTO pkg_index_meta (pkg, version, unit_id, indexed_at, fingerprint) \
  \VALUES (:p, :v, :u, 0, :f) \
  \ON CONFLICT(pkg, version, unit_id) DO UPDATE SET fingerprint = :f"
  [":p" := pkg, ":v" := ver, ":u" := unUnitId unit, ":f" := fp]

-- | Look up every row whose @name@ matches the given symbol.  When
-- the caller supplies a module qualifier, filter by @mod@ too.  Rows
-- come back ordered by @(pkg, mod, version DESC)@ so a name several
-- packages declare answers with a stable candidate list, independent of
-- insertion order.  @version@ is part of the key because it is the only
-- remaining column that distinguishes two rows the cache can hold at
-- once — a package indexed at two versions has a row per version, and
-- without it those tie and fall back to SQLite's unstable sort.
-- Descending so the newest version leads.  This is the SQL-only primitive;
-- 'Hypha.Search.PackageCache.lookupByName' handles qualified-name
-- parsing and project-shadows-global merging.
lookupRowsByName
  :: IndexCache
  -> CacheScope                            -- ^ which versions may answer
  -> Text                                  -- ^ symbol name
  -> Maybe Text                            -- ^ optional module qualifier
  -> IO [VersionedRow]
lookupRowsByName c scope name mMod =
  (reportAnomalies . filter (inScope scope . fst)
                   . map fromStoredVersioned =<<) $ case mMod of
    Nothing ->
      queryNamed (icConn c)
        (Query ("SELECT version, unit_id, " <> rowColumns <> " FROM pkg_index \
                \WHERE name = :n ORDER BY pkg, mod, version DESC"))
        [":n" := name]
    Just modT ->
      queryNamed (icConn c)
        (Query ("SELECT version, unit_id, " <> rowColumns <> " FROM pkg_index \
                \WHERE name = :n AND mod = :m ORDER BY pkg, mod, version DESC"))
        [":n" := name, ":m" := modT]

-- | Every row a component's module presents, whatever the version.
--
-- What a module page needs: for each name the module exposes, the definition
-- site the indexer already resolved — transitively, which is the part no
-- single-hop walk of the imports can reproduce.  Version-free because the
-- caller has a component and a module from a URL and no version to hand —
-- but not configuration-free: a browser inside a project still wants that
-- project's rows, so the scope is asked for even here.
lookupRowsInModule
  :: IndexCache
  -> CacheScope                            -- ^ which configurations may answer
  -> Text                                  -- ^ component key
  -> Text                                  -- ^ module path
  -> IO [IndexRow]
lookupRowsInModule c scope pkg modT =
  map vrRow <$>
    ( (reportAnomalies . filter (inScope scope . fst)
                       . map fromStoredVersioned =<<) $
        queryNamed (icConn c)
          (Query ("SELECT version, unit_id, " <> rowColumns <> " FROM pkg_index \
                  \WHERE pkg = :p AND mod = :m \
                  \ORDER BY version DESC"))
          [":p" := pkg, ":m" := modT] )

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

-- | Read the cached @(pkg, mod, name, sig)@ rows for one configuration of
-- one package version.  Returns @[]@ when no entry exists.
readIndex :: IndexCache -> Text -> Text -> UnitId -> IO [IndexRow]
readIndex c pkg ver unit =
  (reportAnomalies . map fromStored =<<) $ queryNamed (icConn c)
    (Query ("SELECT " <> rowColumns <> " FROM pkg_index \
            \WHERE pkg = :p AND version = :v AND unit_id = :u"))
    [":p" := pkg, ":v" := ver, ":u" := unUnitId unit]

-- | Which of the cache's versions a lookup may answer from.
--
-- The DB is keyed on @(package, version)@ and shared by every project on
-- the host, and a write for a new version does not evict the old one.  So
-- \"what is in the cache\" is the host's indexing history, which is a
-- wider set than \"what this project builds against\" and a narrower one
-- than Hackage.  A caller has to say which of the two it is asking about;
-- there is no sensible default.
-- | An index row together with the version of the @(pkg, version)@ entry
-- it was written under.
--
-- The version stays outside 'IndexRow' because it is a property of the
-- cache entry rather than of the row, and every writer already supplies
-- it separately.  It travels with the row so a caller that has to tell
-- two otherwise-identical rows apart — the same symbol in the same
-- module of the same package, indexed at two versions — can.
data VersionedRow = VersionedRow
  { vrVersion :: !Version
  , vrUnitId  :: !UnitId
    -- ^ The configuration the row was indexed under: cabal's own hash of
    -- the compiler, the resolved dependency unit-ids and the flags.  Two
    -- rows can agree on package, module, name and version and still
    -- disagree, because a @MIN_VERSION_<dep>@ gate resolved differently.
  , vrRow     :: !IndexRow
  }
  deriving stock (Show, Eq, Ord)

data CacheScope
  = ScopePlan !(Map PackageName UnitPin)
    -- ^ Answer only from what the build plan pins.  A row from any other
    -- configuration belongs to some other project that happened to share
    -- this cache, and nothing downstream could tell the two apart.
  | ScopeWholeCache
    -- ^ Answer from every indexed configuration, newest first.  For
    -- callers with no plan to pin to — outside a project, or a module
    -- page that was reached by component and module alone.
  deriving stock (Show, Eq)

-- | How precisely a scope pins one package.
--
-- A plan pins a unit-id, which is the exact configuration.  A
-- @--package-override PKG=VER@ cannot: the user is naming a version the
-- plan does not build, so there is no unit-id to name, and the honest
-- reading of the request is "whatever configuration of that version this
-- machine has".  Keeping the two apart in the type is what stops an
-- override from being silently treated as a plan pin, or a plan pin from
-- being weakened to its version.
data UnitPin
  = PinUnit !UnitId
    -- ^ Exactly this configuration, as the plan resolved it.
  | PinVersion !Version
    -- ^ Any configuration of this version.  Only an override produces
    -- this, and it says so where it is read.
  deriving stock (Show, Eq)

-- | The scope a build plan admits: exactly the configurations it pins.
--
-- Lives here, beside 'CacheScope', so "what the plan admits" has one
-- definition rather than one per caller.
scopeForPlan :: Map PackageName UnitId -> CacheScope
scopeForPlan = ScopePlan . Map.map PinUnit

-- | Whether a stored row's version is one the scope admits.
--
-- The @pkg@ column holds a /component/ key (@pkg@, @pkg:sublib@,
-- @pkg:exe:name@) while a plan pins a version per /package/, so the key
-- is reduced to its package before the comparison.
inScope :: CacheScope -> VersionedRow -> Bool
inScope ScopeWholeCache    _                    = True
inScope (ScopePlan pinned) (VersionedRow v u r) =
    case Map.lookup (packageOfComponent (rowComponent r)) pinned of
      Nothing               -> False
      Just (PinUnit unit)   -> unit == u
      Just (PinVersion ver) -> ver == v
  where
    packageOfComponent = cnPackage . parseComponentName . unComponentKey

-- | The column list, written once so the SELECTs and 'fromStored' cannot
-- drift apart.  Column order is a wire format: it is spelled out rather
-- than derived.
rowColumns :: Text
rowColumns = "pkg, mod, name, sig, def_mod, def_pkg, visibility"

-- | 'fromStored' with the @version@ and @unit_id@ columns kept alongside.
fromStoredVersioned
  :: (Text, Text, Text, Text, Text, Text, Text, Text, Text)
  -> (VersionedRow, Maybe Text)
fromStoredVersioned (ver, unit, pkg, modPath, name, sig, defMod, defPkg, vis) =
  let (r, anomaly) = fromStored (pkg, modPath, name, sig, defMod, defPkg, vis)
  in (VersionedRow (Version ver) (UnitId unit) r, anomaly)

-- | Rebuild a row from its stored columns, along with a description of
-- anything about it we could not make sense of.
--
-- An unrecognised visibility, or a missing definition component, can only
-- come from a row 'ensureIndexFormat' should already have cleared, so the
-- anomaly travels out to 'reportAnomalies' — which is in 'IO' and can
-- actually say something — rather than being silently defaulted here.  The
-- row is kept as 'Internal' (the ranking-neutral choice) and attributed to
-- its own component (the pre-cross-package meaning).
fromStored
  :: (Text, Text, Text, Text, Text, Text, Text) -> (IndexRow, Maybe Text)
fromStored (pkg, modPath, name, sig, defMod, defPkg, vis) =
  ( IndexRow
      { rowComponent  = ComponentKey pkg
      , rowModule     = ModulePath modPath
      , rowName       = SymbolName name
      , rowSignature  = Signature sig
      , rowDefinition = DefinitionRef (ComponentKey definingPkg) (ModulePath defMod)
      , rowVisibility = maybe Internal id (visibilityFromText vis)
      }
  , case (visibilityFromText vis, Text.null defPkg) of
      (Just _,  False) -> Nothing
      (Nothing, _)     -> Just
        (pkg <> "/" <> modPath <> ": unrecognised visibility " <> Text.pack (show vis))
      (Just _,  True)  -> Just
        (pkg <> "/" <> modPath <> ": no definition component; assuming " <> pkg)
  )
  where
    definingPkg = if Text.null defPkg then pkg else defPkg

-- | Trace every anomaly 'fromStored' found, then hand back the rows.
reportAnomalies :: [(a, Maybe Text)] -> IO [a]
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
  -> UnitId                               -- ^ the configuration these rows describe
  -> [IndexRow]
  -> IO ()
writeIndex c pkg ver unit rows = withWrite c $ Sql.withTransaction (icConn c) $ do
  -- Scoped to this configuration.  Deleting every row for
  -- @(pkg, version)@ is what made two projects evict each other: the
  -- same package version resolved against a different compiler or a
  -- different @text@ is a different row set, and both are wanted.
  executeNamed (icConn c)
    "DELETE FROM pkg_index \
    \WHERE pkg = :p AND version = :v AND unit_id = :u"
    [":p" := pkg, ":v" := ver, ":u" := unUnitId unit]
  executeNamed (icConn c)
    "DELETE FROM pkg_index_meta \
    \WHERE pkg = :p AND version = :v AND unit_id = :u"
    [":p" := pkg, ":v" := ver, ":u" := unUnitId unit]
  case rows of
    [] -> pure ()
    _  -> do
      let expanded =
            [ ( unComponentKey (rowComponent r)
              , ver
              , unUnitId unit
              , unModulePath (rowModule r)
              , unSymbolName (rowName r)
              , unSignature (rowSignature r)
              , unModulePath (drModule (rowDefinition r))
              , unComponentKey (drComponent (rowDefinition r))
              , visibilityToText (rowVisibility r)
              )
            | r <- rows
            ]
      executeMany (icConn c)
        "INSERT INTO pkg_index \
        \  (pkg, version, unit_id, mod, name, sig, def_mod, def_pkg \
        \  , visibility) \
        \VALUES (?,?,?,?,?,?,?,?,?)"
        expanded
  -- @strftime('%s','now')@ stores the timestamp as a Unix second so the
  -- meta row stays human-inspectable from a sqlite3 prompt.
  void $ execute (icConn c)
    "INSERT INTO pkg_index_meta (pkg, version, unit_id, indexed_at) \
    \VALUES (?, ?, ?, CAST(strftime('%s','now') AS INTEGER))"
    (pkg, ver, unUnitId unit)
  pruneConfigurations c pkg ver

-- | How many configurations of one @(component, version)@ the cache
-- keeps.
--
-- Configurations accumulate: every project whose plan resolves a package
-- differently adds one, and nothing else would ever remove them.  Three
-- is room for a couple of projects plus a compiler bump, which is the
-- case this exists for, and it bounds the growth a machine-wide cache
-- would otherwise have.  A pruned configuration costs one re-index the
-- next time that project starts, not a wrong answer.
configurationsKept :: Int
configurationsKept = 3

-- | Drop all but the 'configurationsKept' most recently indexed
-- configurations of one @(component, version)@.
--
-- Least-recently-/written/ rather than least-recently-read: the cache
-- records when rows were built and nothing records when they were used,
-- and inventing a read timestamp would mean a write on every lookup.
--
-- @rowid@ breaks ties because @indexed_at@ is a Unix /second/ and a
-- whole indexing run lands inside one: without it, "the newest three"
-- of four configurations written in the same second is decided by
-- whatever order SQLite returns.
--
-- Runs inside the caller's transaction.
pruneConfigurations :: IndexCache -> Text -> Text -> IO ()
pruneConfigurations c pkg ver = do
  keep <- queryNamed (icConn c)
    "SELECT unit_id FROM pkg_index_meta \
    \WHERE pkg = :p AND version = :v \
    \ORDER BY indexed_at DESC, rowid DESC LIMIT :k"
    [":p" := pkg, ":v" := ver, ":k" := configurationsKept]
    :: IO [Only Text]
  stale <- queryNamed (icConn c)
    "SELECT unit_id FROM pkg_index_meta \
    \WHERE pkg = :p AND version = :v"
    [":p" := pkg, ":v" := ver]
    :: IO [Only Text]
  let kept    = [ u | Only u <- keep ]
      dropped = [ u | Only u <- stale, u `notElem` kept ]
  mapM_ dropConfiguration dropped
  where
    dropConfiguration u = do
      executeNamed (icConn c)
        "DELETE FROM pkg_index \
        \WHERE pkg = :p AND version = :v AND unit_id = :u"
        [":p" := pkg, ":v" := ver, ":u" := u]
      executeNamed (icConn c)
        "DELETE FROM pkg_index_meta \
        \WHERE pkg = :p AND version = :v AND unit_id = :u"
        [":p" := pkg, ":v" := ver, ":u" := u]

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
