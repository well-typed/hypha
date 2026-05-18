{-# LANGUAGE OverloadedStrings #-}
module Golden.Package (tests) where

import qualified Data.ByteString.Lazy as LBS
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import qualified Data.Aeson as Aeson

import Hypha.Command.Package (runPackage)
import Hypha.Output.Json (encodeEnvelope)
import Hypha.Project.Discovery (discoverProjectRoot)
import Hypha.Project.Plan (loadBuildPlan)

tests :: TestTree
tests = testGroup "Golden.Package"
  [ goldenVsString
      "package-async produces expected JSON"
      goldenFile
      runPackageCommand
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
            Right outcome -> pure (Aeson.encode (encodeEnvelope "package" outcome))
