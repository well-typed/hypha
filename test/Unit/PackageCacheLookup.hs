{-# LANGUAGE OverloadedStrings #-}
module Unit.PackageCacheLookup (tests) where

import Data.List (sort)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Hypha.Search.Index (IndexRow (..))
import Hypha.Search.PackageCache
  ( CacheOrigin (..), lookupByName, openPackageCacheAt, writeCachedIndex )
import Hypha.Types.ComponentName (ComponentKey (..))
import Hypha.Types.SymbolPath (ModulePath (..))
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

  , testCase "rows come back in deterministic pkg, mod order" $
      -- Inserted in a deliberately non-alphabetical order; the query
      -- must still answer sorted by (pkg, mod) so an agent sees a
      -- stable candidate list regardless of insertion history.
      withSystemTempDirectory "hypha-lk" $ \tmp -> do
        c <- openPackageCacheAt (tmp </> "g.db") Nothing
        writeCachedIndex c OriginGlobal "zebra" "1.0"
          [row "zebra" "Z.Top" "parseJSON" "z"]
        writeCachedIndex c OriginGlobal "aeson" "2.0"
          [ row "aeson" "Data.Aeson.Types" "parseJSON" "a2"
          , row "aeson" "Data.Aeson"       "parseJSON" "a" ]
        writeCachedIndex c OriginGlobal "mango" "1.0"
          [row "mango" "M.Top" "parseJSON" "m"]
        hits <- lookupByName c "parseJSON"
        map (\r -> (unComponentKey (rowComponent r), unModulePath (rowModule r))) hits
          @?= [ ("aeson", "Data.Aeson")
              , ("aeson", "Data.Aeson.Types")
              , ("mango", "M.Top")
              , ("zebra", "Z.Top")
              ]

  , testCase "one package at two versions answers newest first" $
      -- Writes are scoped to (pkg, version), so a package indexed at two
      -- versions keeps a row per version.  (pkg, mod) alone ties them and
      -- leaves the pick to SQLite's unstable sort; the version key breaks
      -- the tie towards the newer row.  Inserted oldest-first so insertion
      -- order and the wanted order disagree.
      withSystemTempDirectory "hypha-lk" $ \tmp -> do
        c <- openPackageCacheAt (tmp </> "g.db") Nothing
        writeCachedIndex c OriginGlobal "HTTP" "4000.4.1"
          [row "HTTP" "Network.HTTP.Base64" "encode" "[Octet] -> String"]
        writeCachedIndex c OriginGlobal "HTTP" "4000.5.0"
          [row "HTTP" "Network.HTTP.Base64" "encode" "[Word8] -> String"]
        hits <- lookupByName c "encode"
        hits @?=
          [ row "HTTP" "Network.HTTP.Base64" "encode" "[Word8] -> String"
          , row "HTTP" "Network.HTTP.Base64" "encode" "[Octet] -> String"
          ]
  ]
