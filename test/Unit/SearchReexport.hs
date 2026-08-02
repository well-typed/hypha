{-# LANGUAGE OverloadedStrings #-}
-- | Coverage for 'Hypha.Search.Reexport'.
--
-- The bug this module exists to kill: resolving a re-export by symbol
-- /name/ gave @Data.IntMap.Lazy.insertWith@ the signature of
-- @Data.Map.insertWith@, because a name-keyed map cannot tell two
-- same-named definitions apart.  Every case below pins a shape where
-- name-keyed resolution would be wrong.
module Unit.SearchReexport (tests) where

import           Data.Text (Text)
import qualified Data.Map.Strict    as Map
import qualified Data.Text          as Text
import qualified Data.Text.IO       as TIO

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Hypha.Search.Reexport
  ( DefinitionSite (..), resolveComponent
  , sharedSegments )
import Hypha.Source.Extensions (defaultLanguageSettings)
import Hypha.Source.Interface
  ( ExportItem (..), ImportItem (..), ModuleInterface (..), parseInterface )
import Hypha.Types.SymbolPath (ModulePath (..), SymbolName (..))

component :: IO [ModuleInterface]
component = mapM load
  [ "test/fixtures/reexport/src/Fixture/Internal.hs"
  , "test/fixtures/reexport/src/Fixture/Facade.hs"
  , "test/fixtures/reexport/src/Fixture/Wrapper.hs"
  , "test/fixtures/reexport/src/Fixture/Strict.hs"
  , "test/fixtures/reexport/src/Fixture/StrictInternal.hs"
  , "test/fixtures/reexport/src/Fixture/Other.hs"
  ]

load :: FilePath -> IO ModuleInterface
load fp = do
  src <- TIO.readFile fp
  case parseInterface defaultLanguageSettings fp src of
    Left e  -> fail ("parse failed for " <> fp <> ": " <> show e)
    Right i -> pure i

-- | Parse a module from an inline source string.  Used where a legal
-- on-disk fixture cannot express the shape under test.
inline :: Text -> Text -> IO ModuleInterface
inline name src =
  case parseInterface defaultLanguageSettings (Text.unpack name <> ".hs") src of
    Left e  -> fail ("parse failed for " <> Text.unpack name <> ": " <> show e)
    Right i -> pure i

siteOf
  :: Map.Map (ModulePath, SymbolName) DefinitionSite
  -> (ModulePath, SymbolName)
  -> IO DefinitionSite
siteOf m k = maybe (fail ("no resolution for " <> show k)) pure (Map.lookup k m)

tests :: TestTree
tests = testGroup "Unit.SearchReexport"
  [ testCase "a locally declared name resolves to itself" $ do
      r <- resolveComponent <$> component
      s <- siteOf r (ModulePath "Fixture.Internal", SymbolName "insertBag")
      s @?= DefinedHere

  , testCase "a re-export resolves to the module that declares it" $ do
      r <- resolveComponent <$> component
      s <- siteOf r (ModulePath "Fixture.Wrapper", SymbolName "insertBag")
      s @?= DefinedIn (ModulePath "Fixture.Internal")

  , testCase "same name, different wrappers, different definitions" $ do
      -- The case a name-keyed map gets wrong: Fixture.Strict.insertBag and
      -- Fixture.Wrapper.insertBag share a name and a signature, and must
      -- NOT share a definition site.
      r  <- resolveComponent <$> component
      s1 <- siteOf r (ModulePath "Fixture.Wrapper", SymbolName "insertBag")
      s2 <- siteOf r (ModulePath "Fixture.Strict",  SymbolName "insertBag")
      s1 @?= DefinedIn (ModulePath "Fixture.Internal")
      s2 @?= DefinedIn (ModulePath "Fixture.StrictInternal")

  , testCase "a two-hop chain resolves to the definition, not the first hop" $ do
      -- Fixture.Facade re-exports Fixture.Wrapper's insertBag, which
      -- Fixture.Wrapper re-exports from Fixture.Internal.  Stopping at the
      -- first hop names a module with no declaration to read, which is how
      -- @hypha source containers/Data.Map/insertWith@ came back empty.
      r <- resolveComponent <$> component
      s <- siteOf r (ModulePath "Fixture.Facade", SymbolName "insertBag")
      s @?= DefinedIn (ModulePath "Fixture.Internal")

  , testCase "module re-export form contributes the target's exports" $ do
      r <- resolveComponent <$> component
      s <- siteOf r (ModulePath "Fixture.Wrapper", SymbolName "otherOnly")
      s @?= DefinedIn (ModulePath "Fixture.Other")

  , testCase "a re-exported name resolves past its same-named sibling" $ do
      -- Fixture.Internal also declares sizeBag, but Fixture.Wrapper
      -- re-exports Fixture.Other's, and that is the one it must get.
      r <- resolveComponent <$> component
      s <- siteOf r (ModulePath "Fixture.Wrapper", SymbolName "sizeBag")
      s @?= DefinedIn (ModulePath "Fixture.Other")

  , testCase "a name no module of the component exports gets no entry" $ do
      r <- resolveComponent <$> component
      Map.lookup (ModulePath "Fixture.Internal", SymbolName "length") r @?= Nothing

  , testCase "ambiguity is resolved by module proximity, and recorded" $ do
      -- Two open imports both declare the name, so the import list does
      -- not decide it: the sibling wins on shared module segments, and
      -- the loser is kept so the choice is inspectable rather than an
      -- accident of list order.
      --
      -- Inline rather than a fixture on purpose: a package that really
      -- had two insertWiths in scope at one export would not compile.
      -- We are pinning what the resolver does with imprecise input, and
      -- imprecise input is what real re-export chains hand it.
      let defines m = inline m
            ("module " <> m <> " (insertWith) where\ninsertWith = undefined\n")
          asking = inline "Data.Map.Strict" $ Text.unlines
            [ "module Data.Map.Strict (insertWith) where"
            , "import Data.Set.Internal"
            , "import Data.Map.Strict.Internal"
            ]
      ifaces <- mapM id
        [ asking
        , defines "Data.Set.Internal"
        , defines "Data.Map.Strict.Internal"
        ]
      site <- siteOf (resolveComponent ifaces)
                (ModulePath "Data.Map.Strict", SymbolName "insertWith")
      -- Proximity decides: Data.Map.Strict.Internal shares two segments
      -- with the asking module, Data.Set.Internal one.
      site @?= DefinedIn (ModulePath "Data.Map.Strict.Internal")

  , testCase "a mutual re-export cycle terminates instead of diverging" $ do
      -- A re-exports x from B while B re-exports x from A.  Neither
      -- declares it, so neither can resolve — but the resolver must
      -- notice the cycle rather than recurse forever.  A test that hangs
      -- here is a failing test.
      let mkIface m imp = ModuleInterface
            { miName      = ModulePath m
            , miExports   = Just [ExportSymbol (SymbolName "x") []]
            , miImports   = [ImportItem (ModulePath imp) Nothing]
            , miDecls     = []
            , miHeaderDoc = Nothing
            }
          r = resolveComponent [mkIface "A" "B", mkIface "B" "A"]
      Map.lookup (ModulePath "A", SymbolName "x") r
        @?= Just (DefinedOutside (ModulePath "B"))
      Map.lookup (ModulePath "B", SymbolName "x") r
        @?= Just (DefinedOutside (ModulePath "A"))

  , testCase "shared segments count segments, not characters" $ do
      -- The predecessor compared a dotted module prefix against slashed
      -- file paths, so Data.Map.Strict and Data.Set.Internal looked
      -- nearly identical and the sweep picked whichever came first.
      sharedSegments (ModulePath "Data.Map.Strict") (ModulePath "Data.Map.Internal") @?= 2
      sharedSegments (ModulePath "Data.Map.Strict") (ModulePath "Data.Set.Internal") @?= 1
      sharedSegments (ModulePath "Data.Map")        (ModulePath "Data.Map")          @?= 2
  ]
