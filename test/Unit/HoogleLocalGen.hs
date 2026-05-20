{-# LANGUAGE OverloadedStrings #-}
module Unit.HoogleLocalGen (tests) where

import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Hypha.Hoogle.Local (scavengeStoreTxt)
import Hypha.Types.PackageId (PackageId (..), PackageName (..), Version (..))

tests :: TestTree
tests = testGroup "Unit.HoogleLocalGen"
  [ testCase "scavenger finds <pkg>.txt under store layout" $
      withSystemTempDirectory "hypha-hg" $ \tmp -> do
        let pid    = PackageId (PackageName "containers") (Version "0.6.7")
            hashD  = tmp </> "containers-0.6.7-abc"
            docDir = hashD </> "share" </> "doc"
                          </> "containers-0.6.7" </> "html"
        createDirectoryIfMissing True docDir
        let txt = docDir </> "containers.txt"
        writeFile txt "@package containers\n"
        path <- scavengeStoreTxt tmp pid
        path @?= Just txt

  , testCase "scavenger returns Nothing when missing" $
      withSystemTempDirectory "hypha-hg" $ \tmp -> do
        let pid = PackageId (PackageName "ghost") (Version "0.0")
        createDirectoryIfMissing True tmp
        path <- scavengeStoreTxt tmp pid
        path @?= Nothing
  ]
