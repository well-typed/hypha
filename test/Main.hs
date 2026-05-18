module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import qualified Golden.Human
import qualified Golden.Package
import qualified Golden.Search
import qualified Golden.Source
import qualified Golden.Symbol
import qualified Golden.Versions
import qualified Property.HackageCache
import qualified Property.HaddockRewrite
import qualified Property.OutputJson
import qualified Property.SymbolPath
import qualified Unit.BuildEnv
import qualified Unit.BuildEnvCompose
import qualified Unit.Deps
import qualified Unit.Doctor
import qualified Unit.Hackage
import qualified Unit.Hoogle
import qualified Unit.Module
import qualified Unit.Project
import qualified Unit.ServerSlots
import qualified Unit.WhatProvides
import qualified Unit.Haddock

main :: IO ()
main = defaultMain (testGroup "hypha"
  [ Golden.Human.tests
  , Golden.Package.tests
  , Golden.Search.tests
  , Golden.Source.tests
  , Golden.Symbol.tests
  , Golden.Versions.tests
  , Property.HaddockRewrite.tests
  , Property.SymbolPath.tests
  , Property.HackageCache.tests
  , Property.OutputJson.tests
  , Unit.BuildEnv.tests
  , Unit.BuildEnvCompose.tests
  , Unit.Deps.tests
  , Unit.Doctor.tests
  , Unit.Project.tests
  , Unit.Hackage.tests
  , Unit.Hoogle.tests
  , Unit.Module.tests
  , Unit.ServerSlots.tests
  , Unit.WhatProvides.tests
  , Unit.Haddock.tests
  ])
