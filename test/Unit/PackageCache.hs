{-# LANGUAGE OverloadedStrings #-}
-- | Two-tier cache: project entries shadow global entries when both
-- carry rows for the same @(pkg, version)@.
module Unit.PackageCache (tests) where

import Data.List (sort)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Hypha.Search.PackageCache
  ( CacheOrigin (..), haveCachedIndex, openPackageCacheAt, readCachedIndex
  , writeCachedIndex )
import Util.Row (row)

tests :: TestTree
tests = testGroup "Unit.PackageCache"
  [ testCase "project entry shadows global for same (pkg, version)" $
      withSystemTempDirectory "hypha-pc" $ \tmp -> do
        let globalDb  = tmp </> "global.db"
            projectDb = tmp </> ".hypha" </> "cache.db"
        c <- openPackageCacheAt globalDb (Just projectDb)
        let pkg = "aeson"
            ver = "2.2.3.0"
            globalRow  = [row pkg "Data.Aeson" "fromJSON" "STORE"]
            projectRow = [row pkg "Data.Aeson" "toJSON"   "FORK"]
        writeCachedIndex c OriginGlobal  pkg ver globalRow
        writeCachedIndex c OriginProject pkg ver projectRow
        rows <- readCachedIndex c pkg ver
        sort rows @?= sort projectRow

  , testCase "global hit visible when project DB has no rows" $
      withSystemTempDirectory "hypha-pc" $ \tmp -> do
        let globalDb  = tmp </> "global.db"
            projectDb = tmp </> ".hypha" </> "cache.db"
        c <- openPackageCacheAt globalDb (Just projectDb)
        let pkg = "containers"
            ver = "0.6.7"
            globalRow = [row pkg "Data.Map.Strict" "fromList" ""]
        writeCachedIndex c OriginGlobal pkg ver globalRow
        present <- haveCachedIndex c pkg ver
        present @?= True
        rows <- readCachedIndex c pkg ver
        sort rows @?= sort globalRow

  , testCase "OriginProject falls back to global when no project DB" $
      withSystemTempDirectory "hypha-pc" $ \tmp -> do
        let globalDb = tmp </> "global.db"
        c <- openPackageCacheAt globalDb Nothing
        let pkg = "text"
            ver = "2.1"
            rows0 = [row pkg "Data.Text" "pack" ""]
        writeCachedIndex c OriginProject pkg ver rows0
        rows <- readCachedIndex c pkg ver
        sort rows @?= sort rows0
  ]
