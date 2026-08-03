{-# LANGUAGE OverloadedStrings #-}
-- | Coverage for the index cache's row shape and its format guard.
--
-- Rows written by older hypha builds carry module names derived from file
-- paths and signatures resolved by symbol name — both wrong, and neither
-- detectable from the row itself.  The format guard is what stops them
-- outliving the fix.
module Unit.SearchIndexCache (tests) where

import qualified Data.IORef as IORef
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Database.SQLite.Simple as Sql
import           System.FilePath ((</>))
import           System.IO.Temp (withSystemTempDirectory)

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Hypha.Search.Cache (openIndexCache, readIndex, writeIndex)
import Hypha.Search.Exports (Export (..), ExportChoice (..), lookupExport)
import Hypha.Search.Index (DefinitionRef (..), IndexRow (..), Visibility (..))
import Hypha.Project.Components (ComponentKind (..))
import Hypha.Search.Indexer (Hydrated (..), hydrateFromCache, indexInputsFingerprint)
import Hypha.Search.PackageCache
  ( CacheOrigin (..), openPackageCacheAt, writeCachedFingerprint
  , writeCachedIndex )
import Hypha.Types.BuildPlan
  ( BuildPlan (..), PackageOrigin (..), PlannedUnit (..), emptyBuildPlan )
import Hypha.Types.PackageId (PackageId (..), PackageName (..), Version (..))
import Hypha.Types.SymbolPath (ModulePath (..), Signature (..), SymbolName (..))
import Util.Row (rowIn)

-- | A wrapper's row: presented by @Data.Map.Strict@, defined in its
-- @.Internal@ sibling.
wrapperRow :: IndexRow
wrapperRow = rowIn
  "containers" "Data.Map.Strict" "insertWith"
  "insertWith :: Ord k => (a -> a -> a) -> k -> a -> Map k a -> Map k a"
  "Data.Map.Strict.Internal" Exposed

tests :: TestTree
tests = testGroup "Unit.SearchIndexCache"
  [ testCase "rows round-trip with their definition module and visibility" $
      withSystemTempDirectory "hypha-cache" $ \dir -> do
        c    <- openIndexCache (dir </> "test.db")
        writeIndex c "containers" "0.7" [wrapperRow]
        rows <- readIndex c "containers" "0.7"
        rows @?= [wrapperRow]
        -- The two new columns are the point: a round-trip that lost them
        -- would still pass an equality on the original four.
        map (drModule . rowDefinition) rows
          @?= [ModulePath "Data.Map.Strict.Internal"]
        map rowVisibility rows @?= [Exposed]

  , testCase "internal visibility survives the round-trip" $
      withSystemTempDirectory "hypha-cache" $ \dir -> do
        c <- openIndexCache (dir </> "vis.db")
        let internal = rowIn "containers" "Data.Map.Internal" "balanceL" ""
                             "Data.Map.Internal" Internal
        writeIndex c "containers" "0.7" [internal]
        rows <- readIndex c "containers" "0.7"
        map rowVisibility rows @?= [Internal]

  , testCase "a pre-format-guard database is cleared on open" $
      withSystemTempDirectory "hypha-cache" $ \dir -> do
        let path = dir </> "old.db"
        -- A generation-1 cache: the old five-column table, no format key,
        -- and a lowercase module name of exactly the kind the path-walking
        -- indexer used to produce.
        conn <- Sql.open path
        Sql.execute_ conn
          "CREATE TABLE pkg_index (pkg TEXT NOT NULL, version TEXT NOT NULL, \
          \mod TEXT NOT NULL, name TEXT NOT NULL, sig TEXT NOT NULL)"
        Sql.execute_ conn
          "CREATE TABLE pkg_index_meta (pkg TEXT NOT NULL, version TEXT NOT NULL, \
          \indexed_at INTEGER NOT NULL, fingerprint TEXT, PRIMARY KEY (pkg, version))"
        Sql.execute_ conn
          "INSERT INTO pkg_index VALUES \
          \('containers','0.7','data.map.strict','insertWith','')"
        Sql.execute_ conn
          "INSERT INTO pkg_index_meta VALUES ('containers','0.7',0,NULL)"
        Sql.close conn

        c    <- openIndexCache path
        rows <- readIndex c "containers" "0.7"
        rows @?= []

  , testCase "opening twice does not clear rows the second time" $
      withSystemTempDirectory "hypha-cache" $ \dir -> do
        let path = dir </> "twice.db"
        c1 <- openIndexCache path
        writeIndex c1 "containers" "0.7" [wrapperRow]
        c2   <- openIndexCache path
        rows <- readIndex c2 "containers" "0.7"
        rows @?= [wrapperRow]

  , testCase "hydration hands back an environment the next unit can resolve against" $
      -- A warm cache holding ghc-internal is exactly how base becomes
      -- resolvable in a run that only rebuilds base.
      withSystemTempDirectory "hypha-hyd" $ \dir -> do
        c <- openPackageCacheAt (dir </> "g.db") Nothing
        writeCachedIndex c OriginGlobal "ghc-internal" "9.1003.0"
          [ rowIn "ghc-internal" "GHC.Internal.Data.Traversable" "mapAccumL"
              "mapAccumL :: Traversable t => (s -> a -> (s, b)) -> s -> t a -> (s, t b)"
              "GHC.Internal.Data.Traversable" Exposed
          ]
        let pid  = PackageId (PackageName "ghc-internal") (Version "9.1003.0")
            plan = emptyBuildPlan
              { bpUnits = Map.singleton (PackageName "ghc-internal") PlannedUnit
                  { puId            = pid
                  , puDeps          = []
                  , puIsLocal       = False
                  , puOrigin        = OriginDistribution
                  , puSrcDir        = Nothing
                  , puDistDir       = Nothing
                  , puLibComponents = []
                  }
              }
        -- Stamped with the digest the build pass computes, because that
        -- is what hydration compares against.  Rows alone are not warmth:
        -- a local package keeps its version across every edit.
        fp <- indexInputsFingerprint plan pid MainLib
        writeCachedFingerprint c OriginGlobal "ghc-internal" "9.1003.0" fp
        ref <- IORef.newIORef []
        hyd <- hydrateFromCache plan c [pid] ref
        hyMissing hyd @?= []
        case lookupExport (Set.singleton (PackageName "ghc-internal"))
               (ModulePath "GHC.Internal.Data.Traversable")
               (SymbolName "mapAccumL") (hyEnv hyd) of
          Just ch -> exSignature (ecChosen ch)
            @?= Signature
                  "mapAccumL :: Traversable t => (s -> a -> (s, b)) -> s -> t a -> (s, t b)"
          Nothing -> fail "cached rows did not reach the environment"
  ]
