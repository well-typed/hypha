{-# LANGUAGE OverloadedStrings #-}
-- | The Haddock module-description header parser: the leading
-- @Key : value@ block that Haddock strips off a module's @-- |@
-- comment before the prose begins.
--
-- Every expectation here is upstream Haddock's behaviour
-- (@Haddock.Interface.ParseModuleHeader@), because the module page has
-- to agree with the Hackage page for the same module.  The two
-- deliberate departures are marked as such.
module Unit.HaddockModuleHeader (tests) where

import qualified Data.Text as Text
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

import Hypha.Haddock.ModuleHeader
  ( FieldName (..), License (..), ModuleHeader (..), parseModuleHeader )
import Hypha.Types.Doc (DocText (..))

-- | A doc comment as GHC hands it to us: markers stripped, so every
-- line of a @-- |@ block carries the single space that followed the
-- marker.
docText :: [Text] -> DocText
docText = DocText . Text.intercalate "\n"

prose :: ModuleHeader -> Maybe Text
prose = fmap unDocText . mhProse

description :: ModuleHeader -> Maybe Text
description = fmap unDocText . mhDescription

tests :: TestTree
tests = testGroup "Unit.HaddockModuleHeader"
  [ testCase "Cabal-style header: fields off the front, prose kept whole" $ do
      let h = parseModuleHeader $ docText
            [ ""
            , " Module      :  Distribution.Simple"
            , " Copyright   :  Isaac Jones 2003-2005"
            , " License     :  BSD3"
            , ""
            , " Maintainer  :  cabal-devel@haskell.org"
            , " Portability :  portable"
            , ""
            , " This is the command line front end to the Simple build system."
            , ""
            , " > runhaskell Setup.hs configure"
            ]
      mhCopyright h   @?= Just "Isaac Jones 2003-2005"
      mhLicense h     @?= Just (LicenseText "BSD3")
      mhMaintainer h  @?= Just "cabal-devel@haskell.org"
      mhPortability h @?= Just "portable"
      mhStability h   @?= Nothing
      description h   @?= Nothing
      -- The blank separator and the first line's indent are consumed by
      -- the preceding field's value munch, exactly as upstream does it.
      prose h @?= Just (Text.intercalate "\n"
        [ "This is the command line front end to the Simple build system."
        , ""
        , " > runhaskell Setup.hs configure"
        ])

  , testCase "Module key is recognised and dropped, never shown" $ do
      let h = parseModuleHeader $ docText
            [ " Module      :  Data.Map.Strict"
            , " Copyright   :  ACME"
            ]
      -- Upstream has no slot for it, and a row repeating the page's own
      -- title earns nothing.
      mhExtra h     @?= []
      mhCopyright h @?= Just "ACME"

  , testCase "Description field is the synopsis, not part of the prose" $ do
      let h = parseModuleHeader $ docText
            [ " Description :  Fast maps from keys to values"
            , " Copyright   :  ACME"
            , ""
            , " Real prose."
            ]
      description h @?= Just "Fast maps from keys to values"
      prose h       @?= Just "Real prose."

  , testCase "continuation lines belong to the field, verbatim" $ do
      let h = parseModuleHeader $ docText
            [ " Copyright   :  (c) Alice 2020"
            , "                (c) Bob 2021"
            , " Maintainer  :  a@b.c"
            , ""
            , " Prose."
            ]
      mhCopyright h  @?= Just "(c) Alice 2020\n                (c) Bob 2021"
      mhMaintainer h @?= Just "a@b.c"
      prose h        @?= Just "Prose."

  , testCase "a blank line inside a field does not end it" $ do
      let h = parseModuleHeader $ docText
            [ " Description :  this is a"
            , "    rather long"
            , ""
            , "    description"
            , ""
            , " Prose starts here."
            ]
      description h @?= Just "this is a\n    rather long\n\n    description"
      prose h       @?= Just "Prose starts here."

  , testCase "unknown keys are kept as rows, in source order" $ do
      -- Departure from upstream, which parses these off the front and
      -- then silently discards them.  We show them; the prose we render
      -- is still byte-for-byte what Hackage renders.
      let h = parseModuleHeader $ docText
            [ " Copyright   :  ACME"
            , " Since       :  1.2"
            , " License     :  BSD3"
            , " Reviewed-By :  nobody"
            , ""
            , " Prose."
            ]
      mhExtra h @?= [ (FieldName "Since", "1.2")
                    , (FieldName "Reviewed-By", "nobody")
                    ]
      mhCopyright h @?= Just "ACME"
      mhLicense h   @?= Just (LicenseText "BSD3")
      prose h       @?= Just "Prose."

  , testCase "SPDX-License-Identifier wins over License and Licence" $ do
      let h = parseModuleHeader $ docText
            [ " License                 :  BSD3"
            , " Licence                 :  BSD3-but-british"
            , " SPDX-License-Identifier :  BSD-3-Clause"
            ]
      mhLicense h @?= Just (Spdx "BSD-3-Clause")
      mhExtra h   @?= []

  , testCase "Licence spelling fills the licence slot" $ do
      let h = parseModuleHeader $ docText [ " Licence     :  BSD3" ]
      mhLicense h @?= Just (LicenseText "BSD3")

  , testCase "no fields at all: everything is prose, verbatim" $ do
      let src = [ " Just plain prose, no fields here."
                , " Second line."
                ]
          h   = parseModuleHeader (docText src)
      mhCopyright h @?= Nothing
      mhExtra h     @?= []
      prose h       @?= Just (Text.intercalate "\n" src)

  , testCase "a prose line with a colon is not a field" $ do
      -- Field names are alphabetic-or-hyphen only, so the space-bearing
      -- \"Note that x\" cannot be a name and the header ends here.
      let src = [ " Note that x: this is prose, not a field." ]
          h   = parseModuleHeader (docText src)
      mhExtra h @?= []
      prose h   @?= Just (Text.intercalate "\n" src)

  , testCase "a key containing a digit is not a field" $ do
      let src = [ " Since2      :  1.2"
                , " Copyright   :  ACME"
                ]
          h   = parseModuleHeader (docText src)
      mhCopyright h @?= Nothing
      mhExtra h     @?= []
      prose h       @?= Just (Text.intercalate "\n" src)

  , testCase "fields with no prose after them" $ do
      let h = parseModuleHeader $ docText
            [ " Copyright   :  ACME"
            , " License     :  BSD3"
            , ""
            ]
      mhCopyright h @?= Just "ACME"
      prose h       @?= Nothing

  , testCase "prose keeps a braced code block intact" $ do
      -- The shape that rules out Cabal-syntax's readFields as the
      -- parser for this job: its layout syntax rejects the braces.
      let h = parseModuleHeader $ docText
            [ " Copyright   :  ACME"
            , ""
            , " Example usage:"
            , ""
            , " > data T = T { x :: Int }"
            ]
      mhCopyright h @?= Just "ACME"
      prose h @?= Just (Text.intercalate "\n"
        [ "Example usage:"
        , ""
        , " > data T = T { x :: Int }"
        ])

  , testCase "a lone Word: opening the prose is eaten, as upstream eats it" $ do
      -- Upstream's field grammar is any alpha-or-hyphen name plus a
      -- colon, so a prose line that is one bare word and a colon parses
      -- as a valueless field.  Hackage loses that word too, and prose
      -- parity is the whole point of porting rather than inventing.  We
      -- keep the field instead of discarding it, so nothing vanishes
      -- from the /data/; whether an empty value earns a table row is the
      -- renderer's call.
      let h = parseModuleHeader $ docText
            [ " Copyright   :  ACME"
            , ""
            , " Example:"
            , ""
            , " > code"
            ]
      mhCopyright h @?= Just "ACME"
      mhExtra h     @?= [(FieldName "Example", "")]
      -- The valueless field's munch eats the blank line and the next
      -- line's indent, as every field's value munch does.
      prose h       @?= Just "> code"

  , testCase "empty header text yields nothing at all" $ do
      let h = parseModuleHeader (DocText "")
      mhCopyright h @?= Nothing
      mhExtra h     @?= []
      prose h       @?= Nothing

  , testCase "repeated key: the first occurrence wins" $ do
      let h = parseModuleHeader $ docText
            [ " Copyright   :  first"
            , " Copyright   :  second"
            ]
      case mhCopyright h of
        Just "first" -> pure ()
        other        -> assertFailure ("expected the first value, got " <> show other)

  , testCase "an unindented header still parses" $ do
      -- Block comments (@{- | ... -}@) reach us without the one-space
      -- indent that @-- |@ leaves behind.
      let h = parseModuleHeader $ docText
            [ "Copyright   :  ACME"
            , "License     :  BSD3"
            , ""
            , "Prose."
            ]
      mhCopyright h @?= Just "ACME"
      mhLicense h   @?= Just (LicenseText "BSD3")
      prose h       @?= Just "Prose."
  ]
