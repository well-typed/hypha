{-# LANGUAGE OverloadedStrings #-}
module Unit.HoogleLocalGen (tests) where

import Data.IORef (modifyIORef, newIORef, readIORef)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Hypha.Hoogle.Local
  ( HaddockError (..), HaddockRequest (..), HaddockRunner (..)
  , HoogleStamp (..), LocalUnit (..), collectTxtForUnit, ensureFresh
  , scavengeStoreTxt )
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

  , testCase "collectTxtForUnit falls back to HaddockRunner when no store .txt" $
      withSystemTempDirectory "hypha-hg" $ \tmp -> do
        called <- newIORef ([] :: [HaddockRequest])
        let runner = HaddockRunner $ \req -> do
              modifyIORef called (req :)
              writeFile (hrOutput req) "@package foo\n"
              pure (Right (hrOutput req))
            lu = LocalUnit
                  { luPkgId   = PackageId (PackageName "foo") (Version "0.1")
                  , luSrcDirs = [tmp </> "src"]
                  , luIsLocal = True
                  }
        createDirectoryIfMissing True (tmp </> "src")
        writeFile (tmp </> "src" </> "Foo.hs") "module Foo where"
        result <- collectTxtForUnit runner tmp "" lu
        case result of
          Right p -> doesFileExist p >>= (@?= True)
          Left (HaddockError e) -> fail (show e)
        seen <- readIORef called
        length seen @?= 1

  , testCase "ensureFresh writes stamp and skips on second call" $
      withSystemTempDirectory "hypha-hg" $ \tmp -> do
        calls <- newIORef (0 :: Int)
        let runner = HaddockRunner $ \req -> do
              modifyIORef calls (+1)
              writeFile (hrOutput req) "@package x\n"
              pure (Right (hrOutput req))
            stamp = HoogleStamp "ph-1" "fp-1"
            dot = tmp </> ".hypha"
        -- empty unit list: no .txt collection at all, only stamping
        ensureFresh runner "" "" dot stamp []
        ensureFresh runner "" "" dot stamp []
        seen <- readIORef calls
        seen @?= 0
        stampOk <- doesFileExist (dot </> "hoogle-stamp")
        stampOk @?= True

  , testCase "collectTxtForUnit uses store .txt when present (no runner call)" $
      withSystemTempDirectory "hypha-hg" $ \tmp -> do
        called <- newIORef ([] :: [HaddockRequest])
        let pid    = PackageId (PackageName "bar") (Version "0.2")
            hashD  = tmp </> "bar-0.2-xyz"
            docDir = hashD </> "share" </> "doc"
                          </> "bar-0.2" </> "html"
        createDirectoryIfMissing True docDir
        writeFile (docDir </> "bar.txt") "@package bar\n"
        let runner = HaddockRunner $ \req -> do
              modifyIORef called (req :)
              pure (Left (HaddockError "must not be called"))
        result <- collectTxtForUnit runner tmp ""
                    (LocalUnit pid [tmp </> "src"] False)
        case result of
          Right p -> p @?= (docDir </> "bar.txt")
          Left e  -> fail (show e)
        seen <- readIORef called
        length seen @?= 0
  ]
