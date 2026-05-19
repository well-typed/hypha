{-# LANGUAGE OverloadedStrings #-}
module Unit.Modules (tests) where

import qualified Data.Text as Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import qualified Hypha.Source.Modules as Modules

tests :: TestTree
tests = testGroup "Unit.Modules"
  [ testCase "parseExposedModules extracts simple module list" $
      let cabal = Text.unlines
            [ "name:                async"
            , "version:             2.2.5"
            , ""
            , "library"
            , "  exposed-modules:"
            , "      Control.Concurrent.Async"
            , "      Control.Concurrent.Async.Exception"
            , "  build-depends:"
            , "      base >=4.5 && <5"
            ]
          expected = ["Control.Concurrent.Async", "Control.Concurrent.Async.Exception"]
      in Modules.parseExposedModules cabal @?= expected

  , testCase "parseExposedModules handles comma-separated single line" $
      let cabal = Text.unlines
            [ "name: async"
            , "version: 2.2.5"
            , "library"
            , "  exposed-modules: Control.Concurrent.Async, Control.Concurrent.Async.Exception"
            ]
          expected = ["Control.Concurrent.Async", "Control.Concurrent.Async.Exception"]
      in Modules.parseExposedModules cabal @?= expected

  , testCase "parseExposedModules handles parenthesised sub-lists" $
      let cabal = Text.unlines
            [ "exposed-modules: Foo(Bar, Baz)"
            , "  Qux"
            ]
          expected = ["Foo", "Qux"]
      in Modules.parseExposedModules cabal @?= expected

  , testCase "parseExposedModules returns empty when no exposed-modules" $
      let cabal = Text.unlines
            [ "name: foo"
            , "version: 1.0"
            , "library"
            , "  build-depends: base"
            ]
      in Modules.parseExposedModules cabal @?= []

  , testCase "parseExposedModules returns empty for empty input" $
      Modules.parseExposedModules "" @?= []

  , testCase "parseExposedModules stops at blank line" $
      let cabal = Text.unlines
            [ "exposed-modules: Foo"
            , ""
            , "  Bar"
            ]
      in Modules.parseExposedModules cabal @?= ["Foo"]

  , testCase "parseExposedModules stops at next field header" $
      let cabal = Text.unlines
            [ "exposed-modules: Foo"
            , "  Bar"
            , "build-depends: base"
            ]
      in Modules.parseExposedModules cabal @?= ["Foo", "Bar"]

  , testCase "parseExposedModules tolerates underscores" $
      let cabal = Text.unlines
            [ "exposed_modules: Foo"
            , "  Bar"
            ]
          expected = ["Foo", "Bar"]
      in Modules.parseExposedModules cabal @?= expected

  , testCase "parseExposedModules handles leading comma on next line" $
      let cabal = Text.unlines
            [ "exposed-modules:"
            , ", Foo"
            , ", Bar"
            ]
          expected = ["Foo", "Bar"]
      in Modules.parseExposedModules cabal @?= expected

  , testCase "parseExposedModules handles real-world-like cabal" $
      let cabal = Text.unlines
            [ "name: async"
            , "version: 2.2.5"
            , "synopsis: Run IO operations asynchronously and wait for results"
            , "license: BSD-3-Clause"
            , "license-file: LICENSE"
            , "author: Simon Marlow"
            , "maintainer: libraries@haskell.org"
            , "bug-reports: https://github.com/haskell/async/issues"
            , "category: Control"
            , "build-type: Simple"
            , "cabal-version: >= 1.10"
            , ""
            , "library"
            , "  default-language: Haskell2010"
            , "  exposed-modules:"
            , "      Control.Concurrent.Async"
            , "  build-depends:"
            , "      base >= 4.3 && < 5"
            , "      stm >= 2.2 && < 3"
            ]
          expected = ["Control.Concurrent.Async"]
      in Modules.parseExposedModules cabal @?= expected
  ]
