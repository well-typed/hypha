{-# LANGUAGE OverloadedStrings #-}
module Unit.Hoogle (tests) where

import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Hypha.Hoogle.Database (dbPath, planHashFile, isStale, HoogleConfig (..))
import Hypha.Types.BuildPlan (ProjectRoot (..))

tests :: TestTree
tests = testGroup "Hoogle"
  [ testCase "planHashFile path layout" $
      planHashFile (ProjectRoot "/tmp/proj") @?= "/tmp/proj/.hypha/plan-hash"

  , testCase "dbPath path layout" $
      dbPath (ProjectRoot "/tmp/proj") @?= "/tmp/proj/.hypha/hoogle.hoo"

  , testCase "hash file round-trip" $ withSystemTempDirectory "hypha-hoogle-test" $ \tmp -> do
      let pf = planHashFile (ProjectRoot tmp)
      createDirectoryIfMissing True (takeDirectory pf)
      BS.writeFile pf (TE.encodeUtf8 "abcd1234")
      raw <- BS.readFile pf
      raw @?= TE.encodeUtf8 "abcd1234"

  , testCase "staleness: missing hash file => stale" $ withSystemTempDirectory "hypha-hoogle-test" $ \tmp -> do
      let cfg = HoogleConfig
            { hgcProjectRoot = ProjectRoot tmp
            , hgcInputDocs = []
            , hgcPlanHash = "abc"
            }
      stale <- isStale cfg
      stale @?= True

  , testCase "staleness: matching hash => fresh" $ withSystemTempDirectory "hypha-hoogle-test" $ \tmp -> do
      let root = ProjectRoot tmp
      createDirectoryIfMissing True (takeDirectory (planHashFile root))
      BS.writeFile (planHashFile root) (TE.encodeUtf8 "abc")
      -- Also need the db file to exist for freshness
      createDirectoryIfMissing True (takeDirectory (dbPath root))
      BS.writeFile (dbPath root) ""
      let cfg = HoogleConfig
            { hgcProjectRoot = root
            , hgcInputDocs = []
            , hgcPlanHash = "abc"
            }
      stale <- isStale cfg
      stale @?= False

  , testCase "staleness: changed hash => stale" $ withSystemTempDirectory "hypha-hoogle-test" $ \tmp -> do
      let root = ProjectRoot tmp
      createDirectoryIfMissing True (takeDirectory (planHashFile root))
      BS.writeFile (planHashFile root) (TE.encodeUtf8 "old-hash")
      createDirectoryIfMissing True (takeDirectory (dbPath root))
      BS.writeFile (dbPath root) ""
      let cfg = HoogleConfig
            { hgcProjectRoot = root
            , hgcInputDocs = []
            , hgcPlanHash = "new-hash"
            }
      stale <- isStale cfg
      stale @?= True
  ]
