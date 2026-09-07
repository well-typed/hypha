{-# LANGUAGE OverloadedStrings #-}
-- | Properties of the module-description header parser: whatever a
-- module author writes as fields comes back as fields, and whatever
-- they write as prose comes back as prose.
module Property.HaddockModuleHeader (tests) where

import qualified Data.Text as Text
import Data.Text (Text)
import qualified Test.Falsify.Generator as Gen
import qualified Test.Falsify.Predicate as P
import Test.Falsify.Property (assert, gen, testFailed)
import qualified Test.Falsify.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Falsify (testProperty)

import Hypha.Haddock.ModuleHeader
  ( License (..), ModuleHeader (..), parseModuleHeader )
import Hypha.Types.Doc (DocText (..))

-- | A field key and the slot it must land in.
knownFields :: [(Text, ModuleHeader -> Maybe Text)]
knownFields =
  [ ("Copyright",   mhCopyright)
  , ("Maintainer",  mhMaintainer)
  , ("Stability",   mhStability)
  , ("Portability", mhPortability)
  , ("License",     fmap licenseText . mhLicense)
  ]
  where
    licenseText (Spdx t)        = t
    licenseText (LicenseText t) = t

-- | Values a module author plausibly writes: no colon (which would
-- start a second field), no newline, no edge whitespace (which the
-- parser strips by design).
genValue :: Gen.Gen Text
genValue = do
  ws <- Gen.list (Range.between (1, 3)) $ Gen.elem
          (   pure "ACME"
           <> pure "portable"
           <> pure "experimental"
           <> pure "a@b.c"
           <> pure "BSD3"
           <> pure "2020-2026")
  pure (Text.unwords ws)

-- | Prose lines that cannot be mistaken for fields: no colons at all,
-- and no leading indent.  Indent is not incidental here \x2014 a line
-- indented past the key column /is/ a continuation of the preceding
-- field, by Haddock's grammar and on Hackage, so a generator that
-- indented prose would be generating fields.
genProse :: Gen.Gen [Text]
genProse = Gen.list (Range.between (1, 4)) $ Gen.elem
    (   pure "This module does a thing."
     <> pure ""
     <> pure "> code sample"
     <> pure "More prose here.")

-- | How many of the known fields appear.  A count rather than the
-- fields themselves: falsify shrinks and shows what it generates, and a
-- slot accessor is a function, which can do neither.
genFieldCount :: Gen.Gen Int
genFieldCount = Gen.inRange (Range.between (1, length knownFields))

-- | Lay the fields and prose out the way a source file does, then hand
-- it over the way GHC does: one leading space per line.
render :: [(Text, Text)] -> [Text] -> DocText
render fields proseLines = DocText . Text.intercalate "\n" $
     [ " " <> k <> " : " <> v | (k, v) <- fields ]
  ++ [ "" ]
  ++ [ " " <> l | l <- proseLines ]

tests :: TestTree
tests = testGroup "Haddock.ModuleHeader"
  [ testProperty "known fields land in their slots" $ do
      n <- gen genFieldCount
      let chosen = take n knownFields
      values <- gen (mapM (const genValue) chosen)
      proseLines <- gen genProse
      let keys   = map fst chosen
          parsed = parseModuleHeader (render (zip keys values) proseLines)
      mapM_ (checkSlot parsed) (zip3 keys (map snd chosen) values)
      -- Every key here is one upstream knows, so nothing is left over.
      assert $ P.eq P..$ ("extra", []) P..$ ("got", mhExtra parsed)

  , testProperty "prose survives the header, ignoring edge whitespace" $ do
      n <- gen genFieldCount
      let chosen = take n knownFields
      values <- gen (mapM (const genValue) chosen)
      proseLines <- gen genProse
      let parsed = parseModuleHeader
                     (render (zip (map fst chosen) values) proseLines)
          -- Against the block as laid out, indents included: the first
          -- prose line's indent is eaten by the last field's value
          -- munch, every later line keeps its own.
          expected = Text.strip (Text.intercalate "\n" [ " " <> l | l <- proseLines ])
          got      = maybe "" (Text.strip . unDocText) (mhProse parsed)
      assert $ P.eq P..$ ("expected", expected) P..$ ("got", got)

  , testProperty "a description with no fields comes back verbatim" $ do
      proseLines <- gen genProse
      let raw    = Text.intercalate "\n" [ " " <> l | l <- proseLines ]
          parsed = parseModuleHeader (DocText raw)
          got    = maybe "" unDocText (mhProse parsed)
      -- Nothing was a field, so not one character may be reinterpreted.
      assert $ P.eq P..$ ("raw", Text.strip raw) P..$ ("got", Text.strip got)
      assert $ P.eq P..$ ("extra", []) P..$ ("got", mhExtra parsed)
  ]
  where
    checkSlot parsed (key, slot, value)
      | slot parsed == Just value = pure ()
      | otherwise = testFailed $
          "field " <> Text.unpack key <> ": expected " <> show value
            <> ", got " <> show (slot parsed)
