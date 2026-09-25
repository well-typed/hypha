{-# LANGUAGE OverloadedStrings #-}
module Unit.SearchQuery (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

import Hypha.Search.Query
  ( QueryError (..), SearchQuery (..), parseSearchQuery, resolveSearchQuery
  , scopeParam )
import Hypha.Types.ComponentName (ComponentKey (..))

tests :: TestTree
tests = testGroup "Unit.SearchQuery"
  [ testGroup "parseSearchQuery" parseTests
  , testGroup "scopeParam"       scopeParamTests
  , testGroup "resolveSearchQuery" resolveTests
  ]

parseTests :: [TestTree]
parseTests =
  [ testCase "a plain query has no scope" $
      parseSearchQuery "Map lookup"
        @?= Right (SearchQuery Nothing "Map lookup")

  , testCase "a leading pkg: token scopes and leaves the rest" $
      parseSearchQuery "pkg:aeson decode"
        @?= Right (SearchQuery (Just (ComponentKey "aeson")) "decode")

  , testCase "the prefix may sit anywhere in the query" $
      parseSearchQuery "Map pkg:containers lookup"
        @?= Right (SearchQuery (Just (ComponentKey "containers")) "Map lookup")

  , testCase "the scope keeps its case, the key is exact" $
      parseSearchQuery "pkg:HUnit assert"
        @?= Right (SearchQuery (Just (ComponentKey "HUnit")) "assert")

  , testCase "component qualifiers survive the prefix" $
      parseSearchQuery "pkg:hypha:exe:hypha-mcp main"
        @?= Right (SearchQuery (Just (ComponentKey "hypha:exe:hypha-mcp")) "main")

  , testCase "a scope with no terms is still a query" $
      parseSearchQuery "pkg:aeson"
        @?= Right (SearchQuery (Just (ComponentKey "aeson")) "")

  , testCase "a bare pkg: is an error, not a search term" $
      parseSearchQuery "pkg: decode" @?= Left ScopeMissingName

  , testCase "repeating the same scope is harmless" $
      parseSearchQuery "pkg:aeson decode pkg:aeson"
        @?= Right (SearchQuery (Just (ComponentKey "aeson")) "decode")

  , testCase "two different scopes conflict" $
      parseSearchQuery "pkg:aeson pkg:text decode"
        @?= Left (ConflictingScopes (ComponentKey "aeson") (ComponentKey "text"))

  , testCase "only a whole pkg: token counts, not an infix" $
      parseSearchQuery "mypkg:foo"
        @?= Right (SearchQuery Nothing "mypkg:foo")
  ]

scopeParamTests :: [TestTree]
scopeParamTests =
  [ testCase "absent is no scope"  $ scopeParam Nothing @?= Nothing
  , testCase "empty is no scope"   $ scopeParam (Just "") @?= Nothing
    -- The toggle's hidden input is always sent; switched off it is empty.
  , testCase "a name is a scope"   $
      scopeParam (Just "aeson") @?= Just (ComponentKey "aeson")
  ]

resolveTests :: [TestTree]
resolveTests =
  [ testCase "q=pkg:foo bar scopes exactly like q=bar&pkg=foo" $
      resolveSearchQuery known Nothing "pkg:foo bar"
        @?= resolveSearchQuery known (Just (ComponentKey "foo")) "bar"

  , testCase "the prefix wins over the toggle" $
      resolveSearchQuery known (Just (ComponentKey "foo")) "pkg:baz bar"
        @?= Right (SearchQuery (Just (ComponentKey "baz")) "bar")

  , testCase "the toggle applies when there is no prefix" $
      resolveSearchQuery known (Just (ComponentKey "foo")) "bar"
        @?= Right (SearchQuery (Just (ComponentKey "foo")) "bar")

  , testCase "a scope outside the plan is reported, not searched" $
      resolveSearchQuery known Nothing "pkg:nope bar"
        @?= Left (UnknownScope (ComponentKey "nope"))

  , testCase "no scope needs no validation" $
      resolveSearchQuery [] Nothing "bar"
        @?= Right (SearchQuery Nothing "bar")
  ]
  where
    known = map ComponentKey ["foo", "baz"]
