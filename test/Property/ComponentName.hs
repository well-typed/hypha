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
      parseComponentName "hypha"
        @?= ComponentName (PackageName "hypha") MainLib
  , testCase "parses pkg:sublib" $
      parseComponentName "hypha:lib-breakdown"
        @?= ComponentName (PackageName "hypha") (SubLib "lib-breakdown")
  , testCase "parses pkg:exe:name" $
      parseComponentName "hypha:exe:hypha-cli"
        @?= ComponentName (PackageName "hypha") (Exe "hypha-cli")
  , testCase "renders main lib" $
      renderComponentName (ComponentName (PackageName "hypha") MainLib)
        @?= "hypha"
  , testCase "renders sublib" $
      renderComponentName
        (ComponentName (PackageName "hypha") (SubLib "lib-breakdown"))
        @?= "hypha:lib-breakdown"
  , testCase "renders exe" $
      renderComponentName
        (ComponentName (PackageName "hypha") (Exe "hypha-cli"))
        @?= "hypha:exe:hypha-cli"
  , testCase "empty sublib suffix collapses to MainLib" $
      parseComponentName "hypha:"
        @?= ComponentName (PackageName "hypha") MainLib
  , testCase "empty exe suffix collapses to MainLib" $
      parseComponentName "hypha:exe:"
        @?= ComponentName (PackageName "hypha") MainLib
  , testCase "disambiguation: pkg:foo is sublib, pkg:exe:foo is exe" $ do
      let a = parseComponentName "pkg:foo"
          b = parseComponentName "pkg:exe:foo"
      renderComponentName a @?= "pkg:foo"
      renderComponentName b @?= "pkg:exe:foo"
  , testProperty "render . parse . render = render (all three kinds)" $ do
      pkg  <- gen (Gen.elem (pure "hypha" <> pure "containers" <> pure "happy"))
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
