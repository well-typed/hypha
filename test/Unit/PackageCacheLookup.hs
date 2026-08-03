{-# LANGUAGE OverloadedStrings #-}
module Unit.PackageCacheLookup (tests) where

import Data.List (sort)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Hypha.Search.PackageCache
  ( CacheOrigin (..), lookupByName, openPackageCacheAt, writeCachedIndex )
import Util.Row (row)

tests :: TestTree
tests = testGroup "Unit.PackageCacheLookup"
  [ testCase "exact name match" $
      withSystemTempDirectory "hypha-lk" $ \tmp -> do
        c <- openPackageCacheAt (tmp </> "g.db") Nothing
        writeCachedIndex c OriginGlobal "containers" "0.6.7"
          [ row "containers" "Data.Map.Strict" "lookup"
              "Ord k => k -> Map k a -> Maybe a"
          , row "containers" "Data.Set" "member"
              "Ord a => a -> Set a -> Bool" ]
        hits <- lookupByName c "lookup"
        sort hits @?=
          [ row "containers" "Data.Map.Strict" "lookup"
              "Ord k => k -> Map k a -> Maybe a" ]

  , testCase "qualified name match" $
      withSystemTempDirectory "hypha-lk" $ \tmp -> do
        c <- openPackageCacheAt (tmp </> "g.db") Nothing
        writeCachedIndex c OriginGlobal "containers" "0.6.7"
          [ row "containers" "Data.Map.Strict" "lookup" "sig1"
          , row "containers" "Data.Map"        "lookup" "sig2" ]
        hits <- lookupByName c "Data.Map.lookup"
        sort hits @?= [row "containers" "Data.Map" "lookup" "sig2"]

  , testCase "cross-package collisions return all" $
      withSystemTempDirectory "hypha-lk" $ \tmp -> do
        c <- openPackageCacheAt (tmp </> "g.db") Nothing
        writeCachedIndex c OriginGlobal "containers" "0.6.7"
          [row "containers" "Data.Map" "lookup" "sig1"]
        writeCachedIndex c OriginGlobal "unordered-containers" "0.2.20"
          [row "unordered-containers" "Data.HashMap.Strict" "lookup" "sig2"]
        hits <- lookupByName c "lookup"
        length hits @?= 2

  , testCase "missing name returns empty" $
      withSystemTempDirectory "hypha-lk" $ \tmp -> do
        c <- openPackageCacheAt (tmp </> "g.db") Nothing
        hits <- lookupByName c "doesNotExist"
        hits @?= []

  , testCase "project rows shadow global on same triple" $
      withSystemTempDirectory "hypha-lk" $ \tmp -> do
        c <- openPackageCacheAt (tmp </> "g.db") (Just (tmp </> "p.db"))
        writeCachedIndex c OriginGlobal  "aeson" "2.2.3"
          [row "aeson" "Data.Aeson" "fromJSON" "STORE"]
        writeCachedIndex c OriginProject "aeson" "2.2.3"
          [row "aeson" "Data.Aeson" "fromJSON" "FORK"]
        hits <- lookupByName c "fromJSON"
        sort hits @?= [row "aeson" "Data.Aeson" "fromJSON" "FORK"]
  ]
