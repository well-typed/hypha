{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
-- | Two-tier package cache: a process-wide global DB under
-- @$XDG_CACHE_HOME/hypha@ for immutable store packages, and an optional
-- project-local DB under @\<project-root\>/.hypha@ for packages whose
-- contents are mutable (local @packages:@ entries and
-- @source-repository-package@ checkouts).
--
-- Callers see one opaque 'HyphaPackageCache'.  Lookups merge both DBs
-- with the rule \"project shadows global\": a hit in the project DB
-- always wins, so a forked @aeson-2.2.3.0@ checkout never collides with
-- the store copy of the same version.
module Hypha.Search.PackageCache
  ( HyphaPackageCache
  , CacheOrigin (..)
  , openPackageCache
  , openPackageCacheAt
  , haveCachedIndex
  , readCachedIndex
  , lookupByName
  , writeCachedIndex
  , readCachedBlob
  , writeCachedBlob
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Set as Set
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), takeDirectory)

import Hypha.Search.Cache
  ( IndexCache, defaultCachePath, haveIndex, lookupRowsByName
  , openIndexCache, readBlob, readIndex, writeBlob, writeIndex )
import Hypha.Types.BuildPlan (ProjectRoot (..))

-- | Tells writers which DB to target.  Reads do not take an origin —
-- they always consult both with project precedence.
data CacheOrigin
  = OriginGlobal   -- ^ Immutable Hackage/store package — safe to share.
  | OriginProject  -- ^ Local or @source-repository-package@ — scope to project.
  deriving stock (Show, Eq)

-- | Bundled handles to the global cache and (optionally) a project
-- cache.  'hpcProject' is 'Nothing' when no @cabal.project@ /
-- @*.cabal@ ancestor could be located; in that case writes targetting
-- 'OriginProject' silently route to the global DB so the hypha server
-- can still operate outside a project tree.
data HyphaPackageCache = HyphaPackageCache
  { hpcGlobal  :: !IndexCache
  , hpcProject :: !(Maybe IndexCache)
  }

-- | Open the global cache at its XDG default and, when a project root
-- is given, the per-project cache at @\<root\>/.hypha/cache.db@.  Both
-- handles share their lifetime with the returned value.
openPackageCache :: Maybe ProjectRoot -> IO HyphaPackageCache
openPackageCache mRoot = do
  globalPath <- defaultCachePath
  openPackageCacheAt globalPath (fmap projectCachePath mRoot)

-- | Variant that lets the caller pin both DB paths explicitly.  Used by
-- the test-suite; production code should prefer 'openPackageCache'.
openPackageCacheAt :: FilePath -> Maybe FilePath -> IO HyphaPackageCache
openPackageCacheAt globalPath mProjectPath = do
  global  <- openIndexCache globalPath
  project <- traverse openProjectAt mProjectPath
  pure HyphaPackageCache { hpcGlobal = global, hpcProject = project }
  where
    openProjectAt path = do
      createDirectoryIfMissing True (takeDirectory path)
      openIndexCache path

projectCachePath :: ProjectRoot -> FilePath
projectCachePath (ProjectRoot r) = r </> ".hypha" </> "cache.db"

-- | Project hit beats global hit.
haveCachedIndex :: HyphaPackageCache -> Text -> Text -> IO Bool
haveCachedIndex c pkg ver =
  case hpcProject c of
    Just p -> do
      here <- haveIndex p pkg ver
      if here then pure True else haveIndex (hpcGlobal c) pkg ver
    Nothing -> haveIndex (hpcGlobal c) pkg ver

-- | Read indexed rows for @(pkg, ver)@.  If the project DB has *any*
-- rows for the pair, they shadow the global DB completely — we never
-- merge partial results from both sides because that would let a stale
-- store entry leak through holes in a freshly rebuilt local checkout.
readCachedIndex
  :: HyphaPackageCache
  -> Text                              -- ^ package name
  -> Text                              -- ^ package version
  -> IO [(Text, Text, Text, Text)]
readCachedIndex c pkg ver =
  case hpcProject c of
    Just p -> do
      rows <- readIndex p pkg ver
      case rows of
        [] -> readIndex (hpcGlobal c) pkg ver
        _  -> pure rows
    Nothing -> readIndex (hpcGlobal c) pkg ver

-- | Find every cached row whose symbol name matches @query@.  The
-- query may be a bare symbol (@lookup@) or fully qualified
-- (@Data.Map.lookup@).  Project rows shadow global rows on the same
-- @(pkg, mod, name)@ triple so a forked checkout overrides the store
-- copy at the same version.
lookupByName
  :: HyphaPackageCache
  -> Text
  -> IO [(Text, Text, Text, Text)]
lookupByName c rawQuery = do
  let (mMod, name) = splitQualified rawQuery
  projectRows <- case hpcProject c of
    Just p  -> lookupRowsByName p name mMod
    Nothing -> pure []
  globalRows  <- lookupRowsByName (hpcGlobal c) name mMod
  pure (mergeShadow projectRows globalRows)

-- | Split @Data.Map.lookup@ into @(Just "Data.Map", "lookup")@.
-- Bare symbols return @(Nothing, sym)@.
splitQualified :: Text -> (Maybe Text, Text)
splitQualified raw =
  case Text.breakOnEnd "." raw of
    (pre, post)
      | Text.null pre -> (Nothing, post)
      | otherwise     -> (Just (Text.dropEnd 1 pre), post)

-- | Project rows take precedence per @(pkg, mod, name)@; global rows
-- fill in any triples the project does not cover.
mergeShadow
  :: [(Text, Text, Text, Text)]
  -> [(Text, Text, Text, Text)]
  -> [(Text, Text, Text, Text)]
mergeShadow project global =
  let key (p, m, n, _) = (p, m, n)
      projectKeys = Set.fromList (map key project)
  in project ++ filter (\r -> not (key r `Set.member` projectKeys)) global

-- | Route a write to the DB picked by 'CacheOrigin'.  When the caller
-- asks for 'OriginProject' but no project cache exists we fall back to
-- the global DB so we never silently drop rows.
writeCachedIndex
  :: HyphaPackageCache
  -> CacheOrigin
  -> Text
  -> Text
  -> [(Text, Text, Text, Text)]
  -> IO ()
writeCachedIndex c origin pkg ver rows =
  writeIndex (selectWrite c origin) pkg ver rows

-- | Generic blob get.  Blobs are global-only for now: they hold
-- cross-project state (plan hashes, embedding fingerprints) and have
-- no per-project semantics.
readCachedBlob :: HyphaPackageCache -> Text -> IO (Maybe Text)
readCachedBlob c = readBlob (hpcGlobal c)

writeCachedBlob :: HyphaPackageCache -> Text -> Text -> IO ()
writeCachedBlob c = writeBlob (hpcGlobal c)

selectWrite :: HyphaPackageCache -> CacheOrigin -> IndexCache
selectWrite c = \case
  OriginGlobal  -> hpcGlobal c
  OriginProject -> case hpcProject c of
    Just p  -> p
    Nothing -> hpcGlobal c
