{-# LANGUAGE OverloadedStrings #-}
-- | The URL space.  Every segment of a hypha page path is a name we do
-- not control, and Haskell allows characters in an operator that change
-- what a URL means rather than merely how it looks.
module Unit.Route (tests) where

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Hypha.Types.ComponentName (ComponentKey (..))
import Hypha.Types.PackageId (PackageName (..))
import Hypha.Types.Route
import Hypha.Types.SymbolPath (ModulePath (..), SymbolName (..))

tests :: TestTree
tests = testGroup "Unit.Route"
  [ testCase "an ordinary symbol path is left readable" $
      symbolHref (ComponentKey "containers") (ModulePath "Data.Map")
                 (SymbolName "insert")
        @?= "/pkg/containers/Data.Map/insert"

  , testCase "'#' is escaped, so the browser does not truncate the path" $
      -- 1470 rows of a real index carry a '#'.  Unescaped, the browser
      -- drops everything from it and asks for the module page instead --
      -- a page that exists, so nothing reports an error.
      symbolHref (ComponentKey "ghc-prim") (ModulePath "GHC.CString")
                 (SymbolName "unpackCString#")
        @?= "/pkg/ghc-prim/GHC.CString/unpackCString%23"

  , testCase "'/' is escaped, so the route keeps its three captures" $
      symbolHref (ComponentKey "filepath") (ModulePath "System.FilePath")
                 (SymbolName "</>")
        @?= "/pkg/filepath/System.FilePath/%3C%2F%3E"

  , testCase "'?' is escaped, so it does not start a query string" $
      symbolHref (ComponentKey "lens") (ModulePath "Control.Lens")
                 (SymbolName "^?")
        @?= "/pkg/lens/Control.Lens/%5E%3F"

  , testCase "a component key keeps its colons" $
      -- Valid in a path segment, and every builder used to disagree about
      -- whether to escape them.
      componentHref (ComponentKey "hypha:exe:hypha")
        @?= "/pkg/hypha:exe:hypha"

  , testCase "a package href names only the package" $
      packageHref (PackageName "containers") @?= "/pkg/containers"

  , testCase "a module href stops at the module" $
      moduleHref (ComponentKey "containers") (ModulePath "Data.Map.Strict")
        @?= "/pkg/containers/Data.Map.Strict"

  , testCase "segments cannot run into one another" $
      -- A '/' inside a segment must not be able to forge a new one.
      hrefFrom ["pkg", "a/b", "c"] @?= "/pkg/a%2Fb/c"
  ]
