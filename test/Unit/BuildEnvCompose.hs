module Unit.BuildEnvCompose (tests) where

import Data.Functor.Identity (Identity, runIdentity)
import Data.Maybe (isNothing)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Hypha.BuildEnv.Compose (composeBuildEnv)
import Hypha.BuildEnv.Type (BuildEnv (..))
import Hypha.Types.PackageId (PackageId (..), PackageName (..), Version (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests = testGroup "BuildEnv.Compose"
  [ testCase "primary wins for shared keys" testPrimaryWins
  , testCase "fallthrough for missing keys" testFallthrough
  , testCase "discoverInstalledPackages is union" testUnion
  ]

mkMockEnv
  :: Set.Set PackageId
  -> (PackageId -> Maybe FilePath)
  -> (PackageId -> Maybe FilePath)
  -> Version
  -> BuildEnv Identity
mkMockEnv !pkgs !srcMap !haddockMap !ver = BuildEnv
  { discoverInstalledPackages = pure pkgs
  , locatePackageSource       = pure . srcMap
  , locateRepoTarball         = \_ -> pure Nothing
  , locateHaddockHtml         = pure . haddockMap
  , ghcVersion                = pure ver
  }

pkgA :: PackageId
pkgA = PackageId (PackageName (Text.pack "pkg-a")) (Version (Text.pack "1.0"))

pkgB :: PackageId
pkgB = PackageId (PackageName (Text.pack "pkg-b")) (Version (Text.pack "2.0"))

pkgC :: PackageId
pkgC = PackageId (PackageName (Text.pack "pkg-c")) (Version (Text.pack "3.0"))

testPrimaryWins :: IO ()
testPrimaryWins = do
  let primary   = mkMockEnv (Set.fromList [pkgA, pkgB])
                            (\p -> if p == pkgA then Just "/primary/a" else Nothing)
                            (\p -> if p == pkgA then Just "/primary/a.html" else Nothing)
                            (Version (Text.pack "9.6"))
      secondary = mkMockEnv (Set.fromList [pkgA, pkgB])
                            (\p -> if p == pkgA then Just "/secondary/a" else Nothing)
                            (\p -> if p == pkgA then Just "/secondary/a.html" else Nothing)
                            (Version (Text.pack "9.8"))
      composed  = composeBuildEnv primary secondary

  let srcA = runIdentity (locatePackageSource composed pkgA)
  srcA @?= Just "/primary/a"

  let htmlA = runIdentity (locateHaddockHtml composed pkgA)
  htmlA @?= Just "/primary/a.html"

  let ghc = runIdentity (ghcVersion composed)
  ghc @?= Version (Text.pack "9.6")

testFallthrough :: IO ()
testFallthrough = do
  let primary   = mkMockEnv (Set.fromList [pkgA]) (const Nothing) (const Nothing) (Version (Text.pack "9.6"))
      secondary = mkMockEnv (Set.fromList [pkgA, pkgB])
                            (\p -> if p == pkgB then Just "/secondary/b" else Nothing)
                            (\p -> if p == pkgB then Just "/secondary/b.html" else Nothing)
                            (Version (Text.pack "9.8"))
      composed  = composeBuildEnv primary secondary

  let srcB = runIdentity (locatePackageSource composed pkgB)
  srcB @?= Just "/secondary/b"

  let htmlB = runIdentity (locateHaddockHtml composed pkgB)
  htmlB @?= Just "/secondary/b.html"

  let srcC = runIdentity (locatePackageSource composed pkgC)
  isNothing srcC @?= True

testUnion :: IO ()
testUnion = do
  let primary   = mkMockEnv (Set.fromList [pkgA, pkgB]) (const Nothing) (const Nothing) (Version (Text.pack "9.6"))
      secondary = mkMockEnv (Set.fromList [pkgB, pkgC]) (const Nothing) (const Nothing) (Version (Text.pack "9.8"))
      composed  = composeBuildEnv primary secondary

  let found = runIdentity (discoverInstalledPackages composed)
  found @?= Set.fromList [pkgA, pkgB, pkgC]
