{-# LANGUAGE OverloadedStrings #-}
module Property.ComponentName (tests) where

import qualified Data.Text as Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)
import Test.Tasty.Falsify (testProperty)
import qualified Test.Falsify.Generator as Gen
import qualified Test.Falsify.Predicate as P
import Test.Falsify.Property (gen, assert)

import Hypha.Project.Components (ComponentKind (..))
import Hypha.Types.ComponentName
  ( ComponentName (..), parseComponentName, renderComponentName )
import Hypha.Types.PackageId (PackageName (..))

tests :: TestTree
tests = testGroup "Property.ComponentName"
  [ testCase "parses simple package" $
      parseComponentName "nike"
        @?= ComponentName (PackageName "nike") MainLib
  , testCase "parses pkg:sublib" $
      parseComponentName "nike:lib-breakdown"
        @?= ComponentName (PackageName "nike") (SubLib "lib-breakdown")
  , testCase "parses pkg:exe:name" $
      parseComponentName "nike:exe:nike-cli"
        @?= ComponentName (PackageName "nike") (Exe "nike-cli")
  , testCase "renders main lib" $
      renderComponentName (ComponentName (PackageName "nike") MainLib)
        @?= "nike"
  , testCase "renders sublib" $
      renderComponentName
        (ComponentName (PackageName "nike") (SubLib "lib-breakdown"))
        @?= "nike:lib-breakdown"
  , testCase "renders exe" $
      renderComponentName
        (ComponentName (PackageName "nike") (Exe "nike-cli"))
        @?= "nike:exe:nike-cli"
  , testCase "empty sublib suffix collapses to MainLib" $
      parseComponentName "nike:"
        @?= ComponentName (PackageName "nike") MainLib
  , testCase "empty exe suffix collapses to MainLib" $
      parseComponentName "nike:exe:"
        @?= ComponentName (PackageName "nike") MainLib
  , testCase "disambiguation: pkg:foo is sublib, pkg:exe:foo is exe" $ do
      let a = parseComponentName "pkg:foo"
          b = parseComponentName "pkg:exe:foo"
      renderComponentName a @?= "pkg:foo"
      renderComponentName b @?= "pkg:exe:foo"
  , testProperty "render . parse . render = render (all three kinds)" $ do
      pkg  <- gen (Gen.elem (pure "nike" <> pure "containers" <> pure "happy"))
      kind <- gen (Gen.elem
                     (   pure MainLib
                      <> pure (SubLib "lib-foo")
                      <> pure (Exe "foo")))
      let cn   = ComponentName (PackageName (Text.pack pkg)) kind
          got  = renderComponentName
                   (parseComponentName (renderComponentName cn))
          want = renderComponentName cn
      assert $ P.eq P..$ ("want", want) P..$ ("got", got)
  ]
