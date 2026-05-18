{-# LANGUAGE OverloadedStrings #-}
module Unit.WhatProvides (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Set as Set

import Hypha.Command.WhatProvides
  ( Provider (..), WhatProvidesResult (..)
  , providerToJSON, whatProvidesResultToJSON, fromHit
  )
import Hypha.Hoogle.Type (HoogleHit (..))

tests :: TestTree
tests = testGroup "WhatProvides"
  [ testCase "fromHit builds correct provider" testFromHit
  , testCase "whatProvidesResultToJSON produces expected shape" testResultJSON
  , testCase "providerToJSON produces expected shape" testProviderJSON
  ]

testFromHit :: IO ()
testFromHit = do
  let hit = HoogleHit
        { hhPackage = "async"
        , hhModule  = "Control.Concurrent.Async"
        , hhName    = "concurrently"
        , hhSig     = "IO a -> IO b -> IO (a, b)"
        , hhDocs    = "Run two IO actions concurrently"
        }
      p = fromHit "concurrently" hit
  pPackage p @?= "async"
  pModule  p @?= "Control.Concurrent.Async"
  pFetch   p @?= "hypha symbol async/Control.Concurrent.Async/concurrently"

testResultJSON :: IO ()
testResultJSON = do
  let result = WhatProvidesResult "concurrently"
        [ Provider "async" "Control.Concurrent.Async"
            "hypha symbol async/Control.Concurrent.Async/concurrently"
        ]
      val = whatProvidesResultToJSON result
      keys = case val of
        Aeson.Object obj -> Set.fromList (map fst (KM.toList obj))
        _                -> Set.empty
  keys @?= Set.fromList ["symbol", "providers"]

testProviderJSON :: IO ()
testProviderJSON = do
  let p = Provider "async" "Control.Concurrent.Async"
        "hypha symbol async/Control.Concurrent.Async/concurrently"
      val = providerToJSON p
      keys = case val of
        Aeson.Object obj -> Set.fromList (map fst (KM.toList obj))
        _                -> Set.empty
  keys @?= Set.fromList ["package", "module", "fetch"]
