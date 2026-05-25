{-# LANGUAGE OverloadedStrings #-}
module Golden.Versions (tests) where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import System.FilePath ((</>))
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty (TestTree, testGroup)

import Hypha.Cli.Types
import Hypha.Command.Versions (runVersions)
import Hypha.Output.Json (encodeEnvelope)
import Hypha.Types.BuildPlan
import Hypha.Types.PackageId (PackageId (..), PackageName (..), Version (..))

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
        { bpUnits = Map.fromList
            [ (PackageName "async", PlannedUnit
                { puId = PackageId (PackageName "async") (Version "2.2.5")
                , puDeps = []
                , puIsLocal = False
                , puOrigin = OriginHackage
                , puSrcDir  = Nothing
                , puDistDir = Nothing
          , puLibComponents = []
                })
            , (PackageName "base", PlannedUnit
                { puId = PackageId (PackageName "base") (Version "4.18.3.0")
                , puDeps = []
                , puIsLocal = False
                , puOrigin = OriginHackage
                , puSrcDir  = Nothing
                , puDistDir = Nothing
          , puLibComponents = []
                })
            ]
        }
      pkgName = PackageName "async"
      result = runVersions plan pkgName
  case result of
    Left _ -> error "Versions command failed unexpectedly"
    Right outcome -> pure (Aeson.encode (encodeEnvelope VersionsCmd outcome))
