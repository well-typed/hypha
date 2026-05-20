{-# LANGUAGE OverloadedStrings #-}
module Unit.PackageCacheFingerprint (tests) where

import Control.Concurrent (threadDelay)
import qualified Data.Text as Text
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import Hypha.Project.Fingerprint (componentFingerprint)

tests :: TestTree
tests = testGroup "Unit.PackageCacheFingerprint"
  [ testCase "deterministic for the same tree" $
      withSystemTempDirectory "hypha-fp" $ \tmp -> do
        let d = tmp </> "src"
        createDirectoryIfMissing True d
        writeFile (d </> "Foo.hs") "module Foo where"
        a <- componentFingerprint [d]
        b <- componentFingerprint [d]
        a @?= b
        assertBool "non-empty" (not (Text.null a))

  , testCase "changes when a file is added" $
      withSystemTempDirectory "hypha-fp" $ \tmp -> do
        let d = tmp </> "src"
        createDirectoryIfMissing True d
        writeFile (d </> "Foo.hs") "module Foo where"
        before <- componentFingerprint [d]
        threadDelay 1100000  -- 1.1s so mtimes can differ on coarse-grained FSes
        writeFile (d </> "Bar.hs") "module Bar where"
        after  <- componentFingerprint [d]
        assertBool "fingerprint changes" (before /= after)

  , testCase "deterministic with multiple dirs reordered" $
      withSystemTempDirectory "hypha-fp" $ \tmp -> do
        let d1 = tmp </> "a"
            d2 = tmp </> "b"
        createDirectoryIfMissing True d1
        createDirectoryIfMissing True d2
        writeFile (d1 </> "A.hs") "module A where"
        writeFile (d2 </> "B.hs") "module B where"
        x <- componentFingerprint [d1, d2]
        y <- componentFingerprint [d2, d1]
        x @?= y

  , testCase "non-empty digest for empty input" $
      withSystemTempDirectory "hypha-fp" $ \tmp -> do
        let d = tmp </> "src"
        createDirectoryIfMissing True d
        fp <- componentFingerprint [d]
        assertBool "non-empty even for empty dir" (not (Text.null fp))
  ]
