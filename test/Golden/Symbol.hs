{-# LANGUAGE OverloadedStrings #-}
module Golden.Symbol (tests) where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson as Aeson
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)

import Hypha.BuildEnv.Type (BuildEnv (..))
import Hypha.Command.Symbol (runSymbol)
import Hypha.Output.Json (encodeEnvelope)
import Hypha.Project.Discovery (discoverProjectRoot)
import Hypha.Project.Plan (loadBuildPlan)
import Hypha.Types.PackageId (Version (..))
import qualified Data.Set as Set

-- | Mock BuildEnv that points to the fixture source directory.
mockBuildEnv :: FilePath -> BuildEnv IO
mockBuildEnv srcDir = BuildEnv
  { discoverInstalledPackages = pure Set.empty
  , locatePackageSource       = \_ -> pure (Just srcDir)
  , locateHaddockHtml         = \_ -> pure Nothing
  , ghcVersion                = pure (Version "9.6.7")
  }

tests :: TestTree
tests = testGroup "Golden.Symbol"
  [ goldenVsString
      "symbol-concurrently produces expected JSON"
      goldenFile
      runSymbolCommand
  ]
  where
    goldenFile = "test" </> "Golden" </> "golden" </> "symbol-concurrently.compact.json"

runSymbolCommand :: IO LBS.ByteString
runSymbolCommand = do
  let fixtureDir = "test" </> "fixtures" </> "tiny-project"
  rootResult <- discoverProjectRoot (Just fixtureDir)
  case rootResult of
    Left err -> error $ "Could not discover project root: " ++ show err
    Right root -> do
      planResult <- loadBuildPlan root
      case planResult of
        Left err -> error $ "Could not load plan: " ++ show err
        Right plan -> do
          let env = mockBuildEnv "test/fixtures/fake-cabal-store/ghc-9.6.7/async-2.2.5-abc123456789/share/async"
          result <- runSymbol env plan "async/Control.Concurrent.Async/concurrently"
          case result of
            Left err  -> error $ "Symbol command failed: " ++ show err
            Right outcome -> pure (Aeson.encode (encodeEnvelope "symbol" outcome))
