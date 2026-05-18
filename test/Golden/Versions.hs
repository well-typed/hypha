{-# LANGUAGE OverloadedStrings #-}
module Golden.Versions (tests) where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson as Aeson
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)

import Hypha.Command.Versions (runVersions)
import Hypha.Output.Json (encodeEnvelope)
import Hypha.Types.BuildPlan (BuildPlan (..), emptyBuildPlan, PackageOverride (..))
import Hypha.Types.PackageId (PackageName (..), Version (..))
import qualified Data.Map.Strict as Map

tests :: TestTree
tests = testGroup "Golden.Versions"
  [ goldenVsString
      "versions-async produces expected JSON"
      goldenFile
      runVersionsCommand
  ]
  where
    goldenFile = "test" </> "Golden" </> "golden" </> "versions-async.compact.json"

runVersionsCommand :: IO LBS.ByteString
runVersionsCommand = do
  -- Create a plan with async pinned to 2.2.5
  let plan = emptyBuildPlan
        { bpPackages = Map.fromList
            [ (PackageName "async", Version "2.2.5")
            , (PackageName "base", Version "4.18.3.0")
            ]
        }
      pkgName = PackageName "async"
      result = runVersions plan pkgName
  case result of
    Left _ -> error "Versions command failed unexpectedly"
    Right outcome -> pure (Aeson.encode (encodeEnvelope "versions" outcome))
