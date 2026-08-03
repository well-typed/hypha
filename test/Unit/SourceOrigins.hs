{-# LANGUAGE OverloadedStrings #-}
-- | Coverage for 'Hypha.Source.Origins': reading a module's export
-- origins out of GHC's own compiled interface.
--
-- The resolver can only rank imports; it cannot know which one supplies
-- a name, because an open @import Prelude@ supplies every name
-- syntactically and none of them in fact.  GHC already knows — it ran the
-- renamer — and it writes the answer into the @.hi@ file, one
-- fully-qualified origin per export.  Everything below pins a shape that
-- appears in a real @ghc --show-iface@ dump of @base@.
module Unit.SourceOrigins (tests) where

import qualified Data.Map.Strict as Map
import qualified Data.Text       as Text
import qualified Data.Text.IO    as TIO

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import Hypha.Source.Origins (ModuleOrigins (..), parseShowIface)
import Hypha.Types.SymbolPath (ModulePath (..), SymbolName (..))

-- | The origins of one captured dump, or a failure the case can report.
originsOf :: FilePath -> IO (Map.Map SymbolName ModulePath)
originsOf fp = do
  dump <- TIO.readFile fp
  case parseShowIface dump of
    Left e   -> fail ("parseShowIface failed for " <> fp <> ": " <> show e)
    Right mo -> pure (moOrigins mo)

concurrent :: IO (Map.Map SymbolName ModulePath)
concurrent = originsOf "test/fixtures/iface/Control.Concurrent.showiface"

bits :: IO (Map.Map SymbolName ModulePath)
bits = originsOf "test/fixtures/iface/Data.Bits.showiface"

tests :: TestTree
tests = testGroup "Unit.SourceOrigins"
  [ testCase "a qualified export names the module that defines it" $ do
      -- The symbol this whole exercise started from: exported by
      -- base:Control.Concurrent, declared in ghc-internal, and blamed on
      -- Prelude by anything that reads only the import list.
      m <- concurrent
      Map.lookup (SymbolName "isCurrentThreadBound") m
        @?= Just (ModulePath "GHC.Internal.Conc.Bound")

  , testCase "an unqualified export belongs to the module itself" $ do
      -- GHC prints the origin only when it differs from the interface's
      -- own module, so a bare name is a local declaration.
      m <- concurrent
      Map.lookup (SymbolName "forkFinally") m
        @?= Just (ModulePath "Control.Concurrent")

  , testCase "a class brings its methods, each with its own origin" $ do
      -- Bits{.&. .|. ...}: the methods are exports too, and they are
      -- exactly what the source parser cannot see (issue 043).
      m <- bits
      Map.lookup (SymbolName "Bits") m  @?= Just (ModulePath "GHC.Internal.Bits")
      Map.lookup (SymbolName ".&.") m   @?= Just (ModulePath "GHC.Internal.Bits")
      Map.lookup (SymbolName "shiftL") m @?= Just (ModulePath "GHC.Internal.Bits")

  , testCase "an operator name keeps the dots that belong to it" $ do
      -- GHC.Internal.Data.Bits..>>. splits into a module and an operator
      -- whose own name starts and ends with a dot.  Splitting on the last
      -- dot instead yields the module GHC.Internal.Data.Bits..>> and the
      -- name "", which is how a naive reader loses every operator.
      m <- bits
      Map.lookup (SymbolName ".>>.") m
        @?= Just (ModulePath "GHC.Internal.Data.Bits")

  , testCase "the export list stops where the next section starts" $ do
      -- "direct module dependencies:" follows the exports and is full of
      -- module-shaped words.  Reading past the section turns each of them
      -- into an export that does not exist.
      m <- concurrent
      assertBool "no package-qualified word leaked in"
        (not (any (Text.isInfixOf ":" . unSymbolName) (Map.keys m)))
      Map.lookup (SymbolName "Control.Concurrent.Chan") m @?= Nothing

  , testCase "output that is not an interface dump is an error, not empty" $ do
      -- A silent empty map would read as "this module exports nothing"
      -- and delete every entry the repair pass was meant to add.
      case parseShowIface "ghc: could not find Foo.hi\n" of
        Left _   -> pure ()
        Right mo -> fail ("expected a failure, got " <> show (Map.size (moOrigins mo))
                            <> " origins")
  ]
