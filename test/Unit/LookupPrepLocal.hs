{-# LANGUAGE OverloadedStrings #-}
-- | Pins the lazy-prep contract on the lookup cascade.
--
-- @loPrepareLocal@ is the action that brings the project's local
-- Hoogle DB up to date.  It used to run unconditionally before every
-- lookup, which made a tier-1 cache hit pay multi-second indexing
-- cost (and emit a couple dozen lines of stderr chatter from the
-- @hoogle@ library).  The cascade now invokes 'loPrepareLocal' /only/
-- when tier 1 misses, on the assumption that a cache hit is cheap
-- and self-contained.
--
-- A regression that re-introduced the pre-flight ensure call would
-- show up here as the prep counter being non-zero on the cache-hit
-- path.
module Unit.LookupPrepLocal (tests) where

import qualified Data.IORef          as IORef
import           System.FilePath     ((</>))
import           System.IO.Temp      (withSystemTempDirectory)

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Hypha.Command.Lookup
  ( LookupOptions (..), runLookup )
import Hypha.Hoogle.Local (openLocalHoogle)
import Hypha.Hoogle.Remote (defaultRemoteOptions, roOffline)
import Hypha.Hoogle.Type   (HoogleQuery (..))
import Hypha.Search.PackageCache
  ( CacheOrigin (..), openPackageCacheAt, writeCachedIndex )

tests :: TestTree
tests = testGroup "Unit.LookupPrepLocal"
  [ testCase "tier-1 cache hit: loPrepareLocal not invoked" $
      withSystemTempDirectory "hypha-prep" $ \tmp -> do
        cache <- openPackageCacheAt (tmp </> "g.db") Nothing
        writeCachedIndex cache OriginGlobal "containers" "0.6.7"
          [("containers", "Data.Map", "lookup", "")]
        hoogle <- openLocalHoogle (tmp </> "dh") (tmp </> "store") (tmp </> "dist")
        prepRef <- IORef.newIORef (0 :: Int)
        let opts = LookupOptions
              { loOffline      = True   -- short-circuit any remote
              , loRemote       = defaultRemoteOptions { roOffline = True }
              , loPrepareLocal = IORef.modifyIORef' prepRef (+ 1)
              }
        _ <- runLookup cache hoogle opts (HoogleQuery "lookup")
        count <- IORef.readIORef prepRef
        count @?= 0

  , testCase "tier-1 cache miss: loPrepareLocal invoked exactly once" $
      withSystemTempDirectory "hypha-prep" $ \tmp -> do
        cache <- openPackageCacheAt (tmp </> "g.db") Nothing
        -- No rows written, so tier 1 will miss.
        hoogle <- openLocalHoogle (tmp </> "dh") (tmp </> "store") (tmp </> "dist")
        prepRef <- IORef.newIORef (0 :: Int)
        let opts = LookupOptions
              { loOffline      = True
              , loRemote       = defaultRemoteOptions { roOffline = True }
              , loPrepareLocal = IORef.modifyIORef' prepRef (+ 1)
              }
        _ <- runLookup cache hoogle opts (HoogleQuery "lookup")
        count <- IORef.readIORef prepRef
        count @?= 1
  ]
