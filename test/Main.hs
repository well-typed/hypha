module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import qualified Golden.Search
import qualified Property.SymbolPath
import qualified Unit.BuildEnv
import qualified Unit.BuildEnvCompose
import qualified Unit.Project

main :: IO ()
main = defaultMain (testGroup "hypha"
  [ Golden.Search.tests
  , Property.SymbolPath.tests
  , Unit.BuildEnv.tests
  , Unit.BuildEnvCompose.tests
  , Unit.Project.tests
  ])
