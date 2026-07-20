{-# LANGUAGE OverloadedStrings #-}
-- | Regression guard for the @embedFile@ / @data-files@ mismatch.
--
-- Historical context: 'Hypha.Server.Assets' embeds a set of @ui/@ files
-- into the binary at compile time via Template Haskell @embedFile@. Those
-- files must also be declared in @data-files@ in @hypha.cabal@, otherwise
-- they are excluded from the source distribution that @cabal v2-install@
-- (and @cabal sdist@) builds from — and the compile-time @embedFile@ splice
-- then fails with e.g.
--
-- > ui/css/components/haddock.css: withBinaryFile: does not exist
--
-- This exact bug shipped once (@haddock.css@ and @theme.js@ were embedded
-- but never added to @data-files@): @cabal build@ passed because the files
-- sit in the working tree, but @cabal v2-install@ failed. This test pins the
-- invariant so it cannot recur silently.
--
-- Both files are read relative to the package root, which is the working
-- directory during @cabal test@ (the same assumption the golden/unit fixture
-- tests rely on).
module Unit.EmbeddedAssets (tests) where

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Set as Set

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

assetsPath, cabalPath :: FilePath
assetsPath = "src/Hypha/Server/Assets.hs"
cabalPath  = "hypha.cabal"

-- | Every @embedFile \"...\"@ path mentioned in a source file.
embedsFromAssets :: Text -> [Text]
embedsFromAssets = go
  where
    marker = "embedFile \""
    go t =
      case T.breakOn marker t of
        (_, rest)
          | T.null rest -> []
          | otherwise ->
              let (path, rest') = T.breakOn "\"" (T.drop (T.length marker) rest)
              in path : go rest'

-- | The paths listed under the @data-files:@ stanza of a cabal file.
--
-- Collects the indented, non-empty lines immediately following the
-- @data-files:@ header, stopping at the first unindented line (the next
-- field or stanza).
dataFilesFromCabal :: Text -> [Text]
dataFilesFromCabal src =
  let ls          = T.lines src
      afterHeader = drop 1 (dropWhile (\l -> T.strip l /= "data-files:") ls)
      indented l  = case T.uncons l of
                      Just (c, _) -> c == ' ' || c == '\t'
                      Nothing     -> False
      block       = takeWhile indented afterHeader
  in filter (not . T.null) (map T.strip block)

tests :: TestTree
tests = testGroup "Unit.EmbeddedAssets"
  [ testCase "every embedFile asset is declared in data-files" $ do
      assetsSrc <- TIO.readFile assetsPath
      cabalSrc  <- TIO.readFile cabalPath
      let embeds    = embedsFromAssets assetsSrc
          dataFiles = Set.fromList (dataFilesFromCabal cabalSrc)
          missing   = filter (`Set.notMember` dataFiles) embeds

      -- Guard against a silently-broken parser: we must actually find embeds.
      assertBool
        "parser found no embedFile assets in Assets.hs — parser likely broken"
        (not (null embeds))

      assertBool
        ( "embedFile assets missing from `data-files` in hypha.cabal. "
       <> "These would be dropped from the sdist and break `cabal install` "
       <> "with a compile-time `does not exist` error. Add them to "
       <> "data-files: " <> show (map T.unpack missing) )
        (null missing)
  ]
