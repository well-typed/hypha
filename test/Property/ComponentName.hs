{-# LANGUAGE OverloadedStrings #-}
module Property.ComponentName (tests) where

import qualified Data.Text as Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)
import Test.Tasty.Falsify (testProperty)
import qualified Test.Falsify.Generator as Gen
import qualified Test.Falsify.Predicate as P
import Test.Falsify.Property (gen, assert)

import Hypha.Types.ComponentName
  ( ComponentName (..), parseComponentName, renderComponentName )
import Hypha.Types.PackageId (PackageName (..))

tests :: TestTree
tests = testGroup "Property.ComponentName"
  [ testCase "parses simple package" $
      parseComponentName "nike"
        @?= ComponentName (PackageName "nike") Nothing
  , testCase "parses pkg:sublib" $
      parseComponentName "nike:lib-breakdown"
        @?= ComponentName (PackageName "nike") (Just "lib-breakdown")
  , testCase "renders simple" $
      renderComponentName (ComponentName (PackageName "nike") Nothing)
        @?= "nike"
  , testCase "renders composite" $
      renderComponentName
        (ComponentName (PackageName "nike") (Just "lib-breakdown"))
        @?= "nike:lib-breakdown"
  , testCase "empty sublib suffix collapses" $
      parseComponentName "nike:"
        @?= ComponentName (PackageName "nike") Nothing
  , testProperty "render . parse . render = render" $ do
      pkg <- gen (Gen.elem (pure "nike" <> pure "containers" <> pure "aeson"))
      sub <- gen (Gen.elem
                    (   pure Nothing
                     <> pure (Just "lib-foo")
                     <> pure (Just "internal")))
      let cn   = ComponentName (PackageName (Text.pack pkg))
                              (fmap Text.pack sub)
          got  = renderComponentName
                   (parseComponentName (renderComponentName cn))
          want = renderComponentName cn
      assert $ P.eq P..$ ("want", want) P..$ ("got", got)
  ]
