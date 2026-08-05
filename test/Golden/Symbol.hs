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
import Hypha.Command.Symbol (runSymbolWith)
import Hypha.Error (HyphaError, errorMessage)
import Hypha.Output.Json (encodeSuccessEnvelope)
import Hypha.Output.Outcome (Outcome)
import Hypha.Source.Dependencies (dependencyReach)
import Hypha.Types.BuildPlan (emptyBuildPlan)
import Hypha.Types.PackageId (Version (..))
import Util.Fixture (asyncDir, asyncId, noOwnerOracle, resolverFor)

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

-- | Drives 'runSymbolWith' — the arm the CLI actually dispatches to.
--
-- Its predecessor drove a second entry point that took a plan and read the
-- module's file directly.  Two ways of answering one question is how the
-- CLI's half of issue #20 stayed green while broken, so the second one is
-- gone and this pins the surviving one.
runSymbolCommand :: IO LBS.ByteString
runSymbolCommand =
  withSystemTempDirectory "hypha-golden-sym" $ \_cacheDir -> do
    result <- runExceptT pipeline
    case result of
      Left err      -> fail ("Symbol golden failed: " <> show (errorMessage err))
      Right outcome -> pure (Aeson.encode (encodeSuccessEnvelope outcome))
  where
    pipeline :: ExceptT HyphaError IO (Outcome Value)
    pipeline = do
      let env      = mockBuildEnv asyncDir
          resolver = resolverFor [(asyncId, asyncDir)]
      ExceptT (liftIO (runSymbolWith env resolver
                        (dependencyReach emptyBuildPlan resolver noOwnerOracle)
                        "async/Control.Concurrent.Async/concurrently"))
