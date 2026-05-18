module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import qualified Property.SymbolPath
import qualified Property.HackageCache
import qualified Unit.Project
import qualified Unit.Hackage

main :: IO ()
main = defaultMain (testGroup "hypha"
  [ Property.SymbolPath.tests
  , Property.HackageCache.tests
  , Unit.Project.tests
  , Unit.Hackage.tests
  ])
