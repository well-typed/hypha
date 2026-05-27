{-# LANGUAGE OverloadedStrings #-}
module Unit.RepoCache (tests) where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy as LBS
import System.Directory
  ( createDirectoryIfMissing, doesDirectoryExist, doesFileExist
  , removeDirectoryRecursive )
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import Hypha.Cabal.RepoCache
  ( TarballError (..), extractTarballGz, locateRepoTarball )
import Hypha.Types.PackageId (PackageId (..), PackageName (..), Version (..))

tests :: TestTree
tests = testGroup "RepoCache"
  [ testCase "extractTarballGz strips top-level dir" testStripTopLevel
  , testCase "extractTarballGz fails on missing tarball" testMissing
  , testCase "extractTarballGz cleans staging on bad archive" testBadArchive
  , testCase "extractTarballGz rejects multi-root archive" testMultiRoot
  , testCase "extractTarballGz overwrites existing destDir" testReextract
  , testCase "locateRepoTarball: absent root → Right Nothing" testLookupAbsentRoot
  , testCase "locateRepoTarball finds tarball under repo dir" testLookupFound
  , testCase "locateRepoTarball misses on wrong version" testLookupMiss
  ]

-- | Build a minimal @.tar.gz@ containing a single @pkg-ver/foo.txt@
-- entry and confirm the extraction strips the wrapper directory.
testStripTopLevel :: IO ()
testStripTopLevel = withSystemTempDirectory "hypha-repocache" $ \tmp -> do
  let stage = tmp </> "stage"
      pkgDir = stage </> "pkg-1.0"
  createDirectoryIfMissing True pkgDir
  writeFile (pkgDir </> "foo.txt") "hello"
  entries <- Tar.pack stage ["pkg-1.0"]
  let tarball = tmp </> "pkg-1.0.tar.gz"
      destDir = tmp </> "out" </> "pkg-1.0"
  LBS.writeFile tarball (GZip.compress (Tar.write entries))
  removeDirectoryRecursive stage
  r <- extractTarballGz tarball destDir
  r @?= Right ()
  ok <- doesFileExist (destDir </> "foo.txt")
  assertBool "foo.txt promoted to destDir" ok
  contents <- readFile (destDir </> "foo.txt")
  contents @?= "hello"

testMissing :: IO ()
testMissing = withSystemTempDirectory "hypha-repocache" $ \tmp -> do
  let tarball = tmp </> "does-not-exist.tar.gz"
      destDir = tmp </> "out"
  r <- extractTarballGz tarball destDir
  case r of
    Left (TarballMissing p) -> p @?= tarball
    other -> fail ("expected TarballMissing, got: " <> show other)

testBadArchive :: IO ()
testBadArchive = withSystemTempDirectory "hypha-repocache" $ \tmp -> do
  let tarball = tmp </> "junk.tar.gz"
      destDir = tmp </> "out"
  LBS.writeFile tarball "this is not a gzip stream"
  r <- extractTarballGz tarball destDir
  case r of
    Left TarballExtractError{} -> pure ()
    other -> fail ("expected TarballExtractError, got: " <> show other)
  -- Staging directory must not be left behind on failure, otherwise a
  -- subsequent run would see a stale half-populated tree and never
  -- recover.
  stagingLeft <- doesDirectoryExist (destDir <> ".staging")
  assertBool "staging cleaned up after failure" (not stagingLeft)

-- | A tarball with two top-level entries must be rejected as a layout
-- error, not silently extracted with one entry overwriting the other.
testMultiRoot :: IO ()
testMultiRoot = withSystemTempDirectory "hypha-repocache" $ \tmp -> do
  let stage = tmp </> "stage"
  createDirectoryIfMissing True (stage </> "a")
  createDirectoryIfMissing True (stage </> "b")
  writeFile (stage </> "a" </> "x.txt") "x"
  writeFile (stage </> "b" </> "y.txt") "y"
  entries <- Tar.pack stage ["a", "b"]
  let tarball = tmp </> "multi.tar.gz"
      destDir = tmp </> "out"
  LBS.writeFile tarball (GZip.compress (Tar.write entries))
  r <- extractTarballGz tarball destDir
  case r of
    Left TarballLayoutError{} -> pure ()
    other -> fail ("expected TarballLayoutError, got: " <> show other)

-- | A second extraction over an already-populated destDir should
-- succeed (overwrites cleanly) and leave no staging directory.
testReextract :: IO ()
testReextract = withSystemTempDirectory "hypha-repocache" $ \tmp -> do
  tarball <- buildTinyTarball tmp "pkg-1.0"
  let destDir = tmp </> "out" </> "pkg-1.0"
  r1 <- extractTarballGz tarball destDir
  r1 @?= Right ()
  r2 <- extractTarballGz tarball destDir
  r2 @?= Right ()
  stagingLeft <- doesDirectoryExist (destDir <> ".staging")
  assertBool "staging cleaned up after re-extract" (not stagingLeft)
  ok <- doesFileExist (destDir </> "foo.txt")
  assertBool "destDir still populated" ok

testLookupAbsentRoot :: IO ()
testLookupAbsentRoot = withSystemTempDirectory "hypha-repocache" $ \tmp -> do
  let root = tmp </> "missing-packages-root"
      pid  = PackageId (PackageName "extra") (Version "1.7.16")
  r <- locateRepoTarball root pid
  r @?= Right Nothing

testLookupFound :: IO ()
testLookupFound = withSystemTempDirectory "hypha-repocache" $ \tmp -> do
  let root = tmp </> "packages"
      pid  = PackageId (PackageName "foo") (Version "1.2.3")
      relDir = root </> "hackage.haskell.org" </> "foo" </> "1.2.3"
      tarball = relDir </> "foo-1.2.3.tar.gz"
  createDirectoryIfMissing True relDir
  LBS.writeFile tarball "placeholder bytes; existence is all that matters"
  r <- locateRepoTarball root pid
  r @?= Right (Just tarball)

testLookupMiss :: IO ()
testLookupMiss = withSystemTempDirectory "hypha-repocache" $ \tmp -> do
  let root = tmp </> "packages"
      pid  = PackageId (PackageName "foo") (Version "9.9.9")
      relDir = root </> "hackage.haskell.org" </> "foo" </> "1.2.3"
      tarball = relDir </> "foo-1.2.3.tar.gz"
  createDirectoryIfMissing True relDir
  LBS.writeFile tarball ""
  r <- locateRepoTarball root pid
  r @?= Right Nothing

-- | Pack a one-file tarball under @pkg-1.0/foo.txt@ for reuse in tests
-- that need a valid archive.
buildTinyTarball :: FilePath -> String -> IO FilePath
buildTinyTarball tmp wrapperName = do
  let stage = tmp </> "stage-" <> wrapperName
      pkgDir = stage </> wrapperName
  createDirectoryIfMissing True pkgDir
  writeFile (pkgDir </> "foo.txt") "hello"
  entries <- Tar.pack stage [wrapperName]
  let tarball = tmp </> (wrapperName <> ".tar.gz")
  LBS.writeFile tarball (GZip.compress (Tar.write entries))
  removeDirectoryRecursive stage
  pure tarball

