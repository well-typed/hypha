{-# LANGUAGE OverloadedStrings #-}
module Golden.Symbol (tests) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT)
import Data.Aeson (Value)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Set qualified as Set
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty (TestTree, testGroup)

import Hypha.BuildEnv.Type (BuildEnv (..))
import Hypha.Cli.Types
import Hypha.Command.Symbol (runSymbol)
import Hypha.Error
  ( HyphaError (DiscoveryFailure, PlanFailure), errorMessage )
import Hypha.Output.Json (encodeSuccessEnvelope)
import Hypha.Output.Outcome (Outcome)
import Hypha.Project.Discovery (discoverProjectRoot)
import Hypha.Project.Plan (loadBuildPlan)
import Hypha.Types (mapEitherIO)
import Hypha.Types.PackageId (Version (..))

-- | Mock BuildEnv that points to the fixture source directory.
mockBuildEnv :: FilePath -> BuildEnv IO
mockBuildEnv srcDir = BuildEnv
  { discoverInstalledPackages = pure Set.empty
  , locatePackageSource       = \_ -> pure (Just srcDir)
  , locateRepoTarball         = \_ -> pure Nothing
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
runSymbolCommand =
  withSystemTempDirectory "hypha-golden-sym" $ \cacheDir -> do
    result <- runExceptT (pipeline cacheDir)
    case result of
      Left err      -> fail ("Symbol golden failed: " <> show (errorMessage err))
      Right outcome -> pure (Aeson.encode (encodeSuccessEnvelope SymbolCmd outcome))
  where
    fixtureDir = "test" </> "fixtures" </> "tiny-project"
    asyncDir   = "test" </> "fixtures"
              </> "fake-cabal-store" </> "ghc-9.6.7"
              </> "async-2.2.5-abc123456789" </> "share" </> "async"

    pipeline :: FilePath -> ExceptT HyphaError IO (Outcome Value)
    pipeline cacheDir = do
      root <- mapEitherIO DiscoveryFailure (discoverProjectRoot (Just fixtureDir))
      plan <- mapEitherIO (PlanFailure root) (loadBuildPlan cacheDir root)
      let env = mockBuildEnv asyncDir
      ExceptT (liftIO (runSymbol env plan "async/Control.Concurrent.Async/concurrently"))
