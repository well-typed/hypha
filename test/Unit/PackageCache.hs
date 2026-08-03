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
  ( CacheOrigin (..), haveFreshIndex, openPackageCacheAt, readCachedFingerprint
  , readCachedIndex
  , writeCachedFingerprint, writeCachedIndex )
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
        writeCachedFingerprint c OriginGlobal pkg ver "fp-1"
        present <- haveFreshIndex c pkg ver "fp-1"
        present @?= True
        rows <- readCachedIndex c pkg ver
        sort rows @?= sort globalRow

  , testCase "rows written under different inputs are not fresh" $
      withSystemTempDirectory "hypha-pc" $ \tmp -> do
        let globalDb = tmp </> "global.db"
        c <- openPackageCacheAt globalDb Nothing
        let pkg = "containers"
            ver = "0.6.7"
        writeCachedIndex c OriginGlobal pkg ver
          [row pkg "Data.Map.Strict" "fromList" ""]
        writeCachedFingerprint c OriginGlobal pkg ver "fp-before"
        -- The version has not moved -- an edited local package never
        -- changes its version -- so presence of a row must not be what
        -- decides this.
        stale <- haveFreshIndex c pkg ver "fp-after"
        stale @?= False

  , testCase "rows written before fingerprints existed are not fresh" $
      withSystemTempDirectory "hypha-pc" $ \tmp -> do
        let globalDb = tmp </> "global.db"
        c <- openPackageCacheAt globalDb Nothing
        let pkg = "containers"
            ver = "0.6.7"
        writeCachedIndex c OriginGlobal pkg ver
          [row pkg "Data.Map.Strict" "fromList" ""]
        unstamped <- haveFreshIndex c pkg ver "fp-1"
        unstamped @?= False

  , testCase "the fingerprint survives the rows it was stamped for" $
      withSystemTempDirectory "hypha-pc" $ \tmp -> do
        let globalDb = tmp </> "global.db"
        c <- openPackageCacheAt globalDb Nothing
        let pkg = "containers"
            ver = "0.6.7"
        -- writeIndex replaces the meta row, so the stamp has to come
        -- second; stamping first loses it and everything rebuilds on
        -- every start.
        writeCachedIndex c OriginGlobal pkg ver
          [row pkg "Data.Map.Strict" "fromList" ""]
        writeCachedFingerprint c OriginGlobal pkg ver "fp-1"
        stored <- readCachedFingerprint c OriginGlobal pkg ver
        stored @?= Just "fp-1"

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
