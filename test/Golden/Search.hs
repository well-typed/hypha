{-# LANGUAGE OverloadedStrings #-}
module Golden.Search (tests) where

import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import qualified Data.Aeson as Aeson

import Hypha.Command.Search (runSearch)
import Hypha.Output.Json (encodeEnvelope)
import Hypha.Types.BuildPlan (emptyBuildPlan)

tests :: TestTree
tests = testGroup "Golden.Search"
  [ goldenVsString
      "search-map-insert produces expected JSON"
      goldenFile
      runSearchCommand
  ]
  where
    goldenFile = "test" </> "Golden" </> "golden" </> "search-map-insert.compact.json"

runSearchCommand :: IO LBS.ByteString
runSearchCommand = do
  let plan = emptyBuildPlan
      query = "Map.insert" :: Text
      extras = [] :: [Text]
      result = runSearch plan query extras
  case result of
    Left _ -> error "Search command failed unexpectedly"
    Right outcome -> pure (Aeson.encode (encodeEnvelope "search" outcome))
