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
  [ testCase "parses main lib + two sublibs + two exes from fixture" $ do
      let root  = "test" </> "fixtures" </> "cabal"
          cabal = root </> "hypha.cabal"
      comps <- parseLibComponents cabal root
      let summary =
            sort [ ( renderKind (ciKind c)
                   , sort (ciHsSourceDirs c)
                   )
                 | c <- comps
                 ]
      summary @?=
        [ ( "exe:hypha-cli",     [root </> "app"] )
        , ( "exe:wrap",         [root </> "app/wrap"] )
        , ( "lib",              [root </> "src"] )
        , ( "sublib:bench",     [root </> "bench-src"] )
        , ( "sublib:internal",  [root </> "internal-src"] )
        ]
  , testCase "missing cabal file returns []" $ do
      res <- parseLibComponents "/does/not/exist.cabal" "/does/not"
      res @?= []
  ]
  where
    renderKind :: ComponentKind -> String
    renderKind MainLib     = "lib"
    renderKind (SubLib s)  = "sublib:" <> Text.unpack s
    renderKind (Exe    s)  = "exe:"    <> Text.unpack s
