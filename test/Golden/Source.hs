{-# LANGUAGE OverloadedStrings #-}
module Golden.Source (tests) where

import qualified Data.ByteString.Lazy as LBS
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)

import Hypha.BuildEnv.Mock (MockBuildEnv (..), emptyMock, mkMockBuildEnv)
import Hypha.Cli.Types (ClientCommandTag (..))
import Hypha.Command.Source (runSource)
import Hypha.Output.Json (EnvelopeOpts (..), encodeOutcomeBytes)
import Hypha.Types.BuildPlan
  ( BuildPlan (..), PackageOrigin (..), PlannedUnit (..), emptyBuildPlan )
import Hypha.Types.PackageId (PackageId (..), PackageName (..), Version (..))
import qualified Data.Map.Strict as Map

tests :: TestTree
tests = testGroup "Golden.Source"
  [ goldenVsString
      "source-async-concurrently produces expected JSON"
      goldenFile
      runSourceCommand
  ]
  where
    goldenFile = "test" </> "Golden" </> "golden" </> "source-async-concurrently.compact.json"

runSourceCommand :: IO LBS.ByteString
runSourceCommand = do
  -- Create a mock build env with async source
  let asyncSrcDir = "test" </> "fixtures" </> "fake-cabal-store" </> "ghc-9.6.7"
                    </> "async-2.2.5-abc123456789" </> "share" </> "async"
      asyncId = PackageId (PackageName "async") (Version "2.2.5")
      mock = emptyMock
        { mockPackages = Map.fromList
            [ (asyncId, (Just asyncSrcDir, Nothing))
            ]
        }
      env = mkMockBuildEnv mock
      plan = emptyBuildPlan
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
            ]
        }
      modPath = "Control.Concurrent.Async" :: Text
      sym = Nothing :: Maybe Text  -- No symbol, just module header

  result <- runSource env plan asyncId modPath sym
  case result of
    Left err -> do
      putStrLn ("Source command failed: " ++ show err)
      error "Source command failed unexpectedly"
    Right outcome -> do
      let opts = EnvelopeOpts
            { eoFull = False
            , eoSelect = []
            , eoPrettyJson = False
            }
      pure (encodeOutcomeBytes opts SourceCmd compactKeys fullKeys outcome)

compactKeys, fullKeys :: Set Text
compactKeys = Set.fromList ["package", "module", "symbol", "path", "line", "snippet"]
fullKeys = Set.fromList ["package", "version", "module", "symbol", "path", "line", "snippet"]
