{-# LANGUAGE OverloadedStrings #-}
-- | Coverage for the index cache's row shape and its format guard.
--
-- Rows written by older hypha builds carry module names derived from file
-- paths and signatures resolved by symbol name — both wrong, and neither
-- detectable from the row itself.  The format guard is what stops them
-- outliving the fix.
module Unit.SearchIndexCache (tests) where

import qualified Database.SQLite.Simple as Sql
import           System.FilePath ((</>))
import           System.IO.Temp (withSystemTempDirectory)

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Hypha.Search.Cache (openIndexCache, readIndex, writeIndex)
import Hypha.Search.Index (IndexRow (..), Visibility (..))
import Hypha.Types.SymbolPath (ModulePath (..))
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
        map rowDefModule rows @?= [ModulePath "Data.Map.Strict.Internal"]
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
  ]
