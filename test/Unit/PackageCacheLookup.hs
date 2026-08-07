{-# LANGUAGE OverloadedStrings #-}
module Unit.PackageCacheLookup (tests) where

import Data.List (sort)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Hypha.Search.Index (IndexRow (..))
import Hypha.Search.PackageCache
  ( CacheOrigin (..), CacheScope (..), VersionedRow (..), lookupByName
  , openPackageCacheAt, writeCachedIndex )
import Hypha.Types.ComponentName (ComponentKey (..))
import Hypha.Types.PackageId (PackageName (..), Version (..))
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
        hits <- map vrRow <$> lookupByName c ScopeWholeCache "lookup"
        sort hits @?=
          [ row "containers" "Data.Map.Strict" "lookup"
              "Ord k => k -> Map k a -> Maybe a" ]

  , testCase "qualified name match" $
      withSystemTempDirectory "hypha-lk" $ \tmp -> do
        c <- openPackageCacheAt (tmp </> "g.db") Nothing
        writeCachedIndex c OriginGlobal "containers" "0.6.7"
          [ row "containers" "Data.Map.Strict" "lookup" "sig1"
          , row "containers" "Data.Map"        "lookup" "sig2" ]
        hits <- map vrRow <$> lookupByName c ScopeWholeCache "Data.Map.lookup"
        sort hits @?= [row "containers" "Data.Map" "lookup" "sig2"]

  , testCase "cross-package collisions return all" $
      withSystemTempDirectory "hypha-lk" $ \tmp -> do
        c <- openPackageCacheAt (tmp </> "g.db") Nothing
        writeCachedIndex c OriginGlobal "containers" "0.6.7"
          [row "containers" "Data.Map" "lookup" "sig1"]
        writeCachedIndex c OriginGlobal "unordered-containers" "0.2.20"
          [row "unordered-containers" "Data.HashMap.Strict" "lookup" "sig2"]
        hits <- map vrRow <$> lookupByName c ScopeWholeCache "lookup"
        length hits @?= 2

  , testCase "missing name returns empty" $
      withSystemTempDirectory "hypha-lk" $ \tmp -> do
        c <- openPackageCacheAt (tmp </> "g.db") Nothing
        hits <- map vrRow <$> lookupByName c ScopeWholeCache "doesNotExist"
        hits @?= []

  , testCase "project rows shadow global on same triple" $
      withSystemTempDirectory "hypha-lk" $ \tmp -> do
        c <- openPackageCacheAt (tmp </> "g.db") (Just (tmp </> "p.db"))
        writeCachedIndex c OriginGlobal  "aeson" "2.2.3"
          [row "aeson" "Data.Aeson" "fromJSON" "STORE"]
        writeCachedIndex c OriginProject "aeson" "2.2.3"
          [row "aeson" "Data.Aeson" "fromJSON" "FORK"]
        hits <- map vrRow <$> lookupByName c ScopeWholeCache "fromJSON"
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
        hits <- map vrRow <$> lookupByName c ScopeWholeCache "parseJSON"
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
      --
      -- This is the /unpinned/ scope: with no project there is no plan to
      -- pin to, so every indexed version is a legitimate answer.
      withSystemTempDirectory "hypha-lk" $ \tmp -> do
        c <- openPackageCacheAt (tmp </> "g.db") Nothing
        writeCachedIndex c OriginGlobal "HTTP" "4000.4.1"
          [row "HTTP" "Network.HTTP.Base64" "encode" "[Octet] -> String"]
        writeCachedIndex c OriginGlobal "HTTP" "4000.5.0"
          [row "HTTP" "Network.HTTP.Base64" "encode" "[Word8] -> String"]
        hits <- map vrRow <$> lookupByName c ScopeWholeCache "encode"
        hits @?=
          [ row "HTTP" "Network.HTTP.Base64" "encode" "[Word8] -> String"
          , row "HTTP" "Network.HTTP.Base64" "encode" "[Octet] -> String"
          ]

  , testCase "a hit carries the version it was indexed under" $
      -- Two rows for the same symbol, module and package differ only in
      -- the version of the entry they were written under, which is not a
      -- column of the row.  Unless it travels alongside, the pair reaches
      -- the renderer indistinguishable and prints twice.
      withSystemTempDirectory "hypha-lk" $ \tmp -> do
        c <- openPackageCacheAt (tmp </> "g.db") Nothing
        writeCachedIndex c OriginGlobal "HTTP" "4000.4.1"
          [row "HTTP" "Network.HTTP.Base64" "encode" "[Octet] -> String"]
        writeCachedIndex c OriginGlobal "HTTP" "4000.5.0"
          [row "HTTP" "Network.HTTP.Base64" "encode" "[Word8] -> String"]
        hits <- lookupByName c ScopeWholeCache "encode"
        map vrVersion hits @?= [Version "4000.5.0", Version "4000.4.1"]

  , testCase "a version the plan does not pin does not answer" $
      -- The global cache accumulates a row set per (pkg, version) and
      -- never evicts an older version, so a package indexed by some
      -- other project on the same machine is present at a version this
      -- plan does not build against.  Tier 1 must not offer it: the
      -- answer would depend on the machine's history rather than on the
      -- project, and nothing in the output distinguishes the two rows.
      withSystemTempDirectory "hypha-lk" $ \tmp -> do
        c <- openPackageCacheAt (tmp </> "g.db") Nothing
        writeCachedIndex c OriginGlobal "base-compat" "0.14.1"
          [row "base-compat" "Prelude.Compat" "fmap" "OLD"]
        writeCachedIndex c OriginGlobal "base-compat" "0.15.0"
          [row "base-compat" "Prelude.Compat" "fmap" "NEW"]
        hits <- map vrRow <$> lookupByName c (planScope [("base-compat", "0.15.0")]) "fmap"
        hits @?= [row "base-compat" "Prelude.Compat" "fmap" "NEW"]

  , testCase "a package the plan does not mention does not answer" $
      -- Reaching past the plan is the remote tier's job, and it says so
      -- in the tier label.  A cache row for a package this project does
      -- not depend on would arrive labelled 'cache', i.e. claiming to
      -- come from the plan.
      withSystemTempDirectory "hypha-lk" $ \tmp -> do
        c <- openPackageCacheAt (tmp </> "g.db") Nothing
        writeCachedIndex c OriginGlobal "aeson" "2.2.5.0"
          [row "aeson" "Data.Aeson" "encode" "a -> ByteString"]
        hits <- map vrRow <$> lookupByName c (planScope [("containers", "0.6.8")]) "encode"
        hits @?= []

  , testCase "a component key is pinned by its package's plan version" $
      -- The @pkg@ column holds a component key, so a sublibrary or an
      -- executable is stored under @pkg:lib@ / @pkg:exe:name@ while the
      -- plan pins a version per /package/.  Matching the raw key against
      -- the plan would drop every non-main-library row.
      withSystemTempDirectory "hypha-lk" $ \tmp -> do
        c <- openPackageCacheAt (tmp </> "g.db") Nothing
        writeCachedIndex c OriginGlobal "ansi-terminal:exe:ansi-terminal-example" "1.1.5"
          [row "ansi-terminal:exe:ansi-terminal-example" "Main" "main" "IO ()"]
        writeCachedIndex c OriginGlobal "ansi-terminal:exe:ansi-terminal-example" "1.0.2"
          [row "ansi-terminal:exe:ansi-terminal-example" "Main" "main" "IO ()"]
        hits <- map vrRow <$> lookupByName c (planScope [("ansi-terminal", "1.1.5")]) "main"
        length hits @?= 1
  ]

-- | A plan scope from @(package, version)@ pairs.
planScope :: [(Text, Text)] -> CacheScope
planScope =
  ScopePlan . Map.fromList . map (\(p, v) -> (PackageName p, Version v))
