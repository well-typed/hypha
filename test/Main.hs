module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import qualified Golden.Package
import qualified Golden.Search
import qualified Golden.Source
import qualified Golden.Symbol
import qualified Golden.Versions
import qualified Property.HackageCache
import qualified Property.OutputJson
import qualified Property.SymbolPath
import qualified Unit.BuildEnv
import qualified Unit.BuildEnvCompose
import qualified Unit.Hackage
import qualified Unit.Hoogle
import qualified Unit.Module
import qualified Unit.Project

main :: IO ()
main = defaultMain (testGroup "hypha"
  [ Golden.Package.tests
  , Golden.Search.tests
  , Golden.Source.tests
  , Golden.Symbol.tests
  , Golden.Versions.tests
  , Property.SymbolPath.tests
  , Property.HackageCache.tests
  , Property.OutputJson.tests
  , Unit.BuildEnv.tests
  , Unit.BuildEnvCompose.tests
  , Unit.Project.tests
  , Unit.Hackage.tests
  , Unit.Hoogle.tests
  , Unit.Module.tests
  ])
