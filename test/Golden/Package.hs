{-# LANGUAGE OverloadedStrings #-}
module Golden.Package (tests) where

import qualified Data.ByteString.Lazy as LBS
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import qualified Data.Aeson as Aeson

import Hypha.BuildEnv.Type (BuildEnv (..))
import Hypha.Cli.Types (ClientCommandTag (..))
import qualified Hypha.BuildEnv.Cabal as Cabal
import Hypha.Command.Package (runPackage, mkSuccessOutcome)
import Hypha.Hackage.Api (mkOfflineHackageClient)
import Hypha.Output.Json (encodeEnvelope)
import Hypha.Package.Resolver (PackageResolver (..), ResolvedPackage (..), mkPackageResolver)
import Hypha.Project.Discovery (discoverProjectRoot)
import Hypha.Project.Plan (loadBuildPlan)
import qualified Hypha.Source.Modules as SourceModules
import Hypha.Types.PackageId (PackageName (..), PackageId (..), Version (..))
import qualified Data.Set as Set
import qualified Data.Text as Text
import System.Directory (canonicalizePath)

tests :: TestTree
tests = testGroup "Golden.Package"
  [ goldenVsString
      "package-async produces expected JSON"
      goldenFile
      runPackageCommand
  , goldenVsString
      "package-local produces expected JSON"
      ("test" </> "Golden" </> "golden" </> "package-local.compact.json")
      runPackageLocalCommand
  ]
  where
    goldenFile = "test" </> "Golden" </> "golden" </> "package-async.compact.json"

runPackageCommand :: IO LBS.ByteString
runPackageCommand = do
  let fixtureDir = "test" </> "fixtures" </> "tiny-project"
  rootResult <- discoverProjectRoot (Just fixtureDir)
  case rootResult of
    Left err -> error $ "Could not discover project root: " ++ show err
    Right root -> do
      planResult <- loadBuildPlan root
      case planResult of
        Left err -> error $ "Could not load plan: " ++ show err
        Right plan -> do
          case runPackage plan "async" of
            Left err  -> error $ "Package command failed: " ++ show err
            Right outcome -> pure (Aeson.encode (encodeEnvelope PackageCmd outcome))

runPackageLocalCommand :: IO LBS.ByteString
runPackageLocalCommand = do
  let fixtureDir = "test" </> "fixtures" </> "tiny-project"
  rootResult <- discoverProjectRoot (Just fixtureDir)
  case rootResult of
    Left err -> error $ "Could not discover project root: " ++ show err
    Right root -> do
      planResult <- loadBuildPlan root
      case planResult of
        Left err -> error $ "Could not load plan: " ++ show err
        Right plan -> do
          storeDir <- canonicalizePath ("test" </> "fixtures" </> "fake-cabal-store" </> "ghc-9.6.7")
          eEnv <- Cabal.mkCabalBuildEnv storeDir
          env <- case eEnv of
            Right be -> pure be
            Left _   -> pure nullBuildEnv
          hclient <- mkOfflineHackageClient
          resolver <- mkPackageResolver env hclient plan
          result <- resolvePkg resolver (PackageName "mylib")
          case result of
            Left err -> error $ "resolve mylib: " ++ show err
            Right rp -> do
              modules <- resolveLocalModules resolver env (rpPkgId rp)
              let oc = mkSuccessOutcome
                        "mylib"
                        (pkgVersion (rpPkgId rp))
                        (rpIsLocal rp)
                        (rpDepsCount rp)
                        (rpOrigin rp)
                        modules
              pure (Aeson.encode (encodeEnvelope PackageCmd oc))

-- | Resolve modules for a local package by finding its source dir
-- and parsing the .cabal file.  Uses the plan's puSrcDir for speed.
resolveLocalModules :: PackageResolver IO -> BuildEnv IO -> PackageId -> IO [Text.Text]
resolveLocalModules resolver _env pid = do
  -- Try resolveSrc (which checks plan source dir first, then store, then Hackage).
  eDir <- resolveSrc resolver pid
  case eDir of
    Left _err -> pure []
    Right dir -> SourceModules.getExposedModules dir

-- | A minimal BuildEnv that finds nothing — used when the cabal store
-- is unreachable.
nullBuildEnv :: BuildEnv IO
nullBuildEnv = BuildEnv
  { discoverInstalledPackages = pure Set.empty
  , locatePackageSource       = \_ -> pure Nothing
  , locateHaddockHtml         = \_ -> pure Nothing
  , ghcVersion                = pure (Version "unknown")
  }
