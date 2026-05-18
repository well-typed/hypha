module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import qualified Property.SymbolPath

main :: IO ()
main = defaultMain (testGroup "hypha"
  [ Property.SymbolPath.tests
  ])
