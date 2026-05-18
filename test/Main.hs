module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import qualified Property.OutputJson
import qualified Property.SymbolPath
import qualified Property.HackageCache
import qualified Unit.BuildEnv
import qualified Unit.BuildEnvCompose
import qualified Unit.Project
import qualified Unit.Hackage
import qualified Unit.Hoogle

main :: IO ()
main = defaultMain (testGroup "hypha"
  [ Property.SymbolPath.tests
  , Property.HackageCache.tests
  , Property.OutputJson.tests
  , Unit.BuildEnv.tests
  , Unit.BuildEnvCompose.tests
  , Unit.Project.tests
  , Unit.Hackage.tests
  , Unit.Hoogle.tests
  ])
