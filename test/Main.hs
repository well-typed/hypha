module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import qualified Property.SymbolPath
import qualified Unit.BuildEnv
import qualified Unit.Project

main :: IO ()
main = defaultMain (testGroup "hypha"
  [ Property.SymbolPath.tests
  , Unit.BuildEnv.tests
  , Unit.Project.tests
  ])
