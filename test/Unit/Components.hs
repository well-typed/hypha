{-# LANGUAGE OverloadedStrings #-}
module Unit.Components (tests) where

import Data.List (sort)
import qualified Data.Text as Text
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Hypha.Project.Components
  ( ComponentInfo (..), ComponentKind (..), parseLibComponents )

tests :: TestTree
tests = testGroup "Unit.Components"
  [ testCase "parses main lib + two sublibs from fixture" $ do
      let root  = "test" </> "fixtures" </> "cabal"
          cabal = root </> "nike.cabal"
      comps <- parseLibComponents cabal root
      let toSublib MainLib    = Nothing
          toSublib (SubLib s) = Just (Text.unpack s)
          toSublib (Exe _)    = Nothing
          summary =
            sort [ ( toSublib (ciKind c)
                   , sort (ciHsSourceDirs c)
                   )
                 | c <- comps
                 ]
      summary @?=
        [ ( Nothing,         [root </> "src"] )
        , ( Just "bench",    [root </> "bench-src"] )
        , ( Just "internal", [root </> "internal-src"] )
        ]
  , testCase "missing cabal file returns []" $ do
      res <- parseLibComponents "/does/not/exist.cabal" "/does/not"
      res @?= []
  ]
