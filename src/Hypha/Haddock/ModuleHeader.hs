{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
-- | The structured header of a Haddock module description: the leading
-- @Key : value@ block that precedes the prose.
--
-- > -- |
-- > -- Module      :  Distribution.Simple
-- > -- Copyright   :  Isaac Jones 2003-2005
-- > -- License     :  BSD3
-- > --
-- > -- This is the command line front end to the Simple build system.
--
-- Rendering that block as prose is what issue #46 was about: a reader
-- sees @Module : \x2026 Copyright : \x2026@ glued into one paragraph.
--
-- No library on Hackage parses this.  @haddock-library@ (which we
-- already use for the markup itself) exposes only @Doc@, @Markup@,
-- @Parser@ and @Types@; the parser lives in @haddock-api@'s internal
-- @Haddock.Interface.ParseModuleHeader@, which is not exposed and which
-- pins the @ghc@ library to one exact compiler \x2014 hypha is
-- deliberately compiler-independent via @ghc-lib-parser@.
-- @Cabal-syntax@'s @Distribution.Fields.readFields@ looks like a fit and
-- is not: its layout syntax rejects a braced code block in the prose,
-- and it has no notion of stopping and handing the remainder back
-- untouched.
--
-- So this is a port of that upstream parser \x2014 originally
-- @Haddock.Interface.ParseModuleHeader@, (c) Simon Marlow 2006, Isaac
-- Dupree 2009, BSD-like \x2014 kept faithful so that the module page
-- agrees with the Hackage page for the same module.
module Hypha.Haddock.ModuleHeader
  ( ModuleHeader (..)
  , License (..)
  , FieldName (..)
  , parseModuleHeader
  ) where

import Control.Applicative ((<|>))
import Data.Char (isAlpha, isSpace)
import Data.Text (Text)
import qualified Data.Text as Text

import Hypha.Types.Doc (DocText (..))

-- | A field key, as written in the source.  Alphabetic characters and
-- hyphens only \x2014 upstream's name grammar, which is why a prose line
-- like @Note that x: \x2026@ cannot masquerade as a field.
newtype FieldName = FieldName { unFieldName :: Text }
  deriving stock (Show, Eq, Ord)

-- | The licence slot.  Upstream collapses the SPDX identifier and the
-- free-text spellings into one 'String' and loses which it was; keeping
-- them apart lets the page link an SPDX id to its definition.
data License
  = Spdx !Text
    -- ^ From @SPDX-License-Identifier@.
  | LicenseText !Text
    -- ^ From @License@ or @Licence@.
  deriving stock (Show, Eq)

-- | A parsed module description: the fields upstream recognises, the
-- ones it drops, and the prose that follows.
data ModuleHeader = ModuleHeader
  { mhDescription :: !(Maybe DocText)
    -- ^ The @Description@ field: Haddock's one-line synopsis for the
    -- module, which it shows in package indexes rather than on the
    -- module page itself.
  , mhCopyright   :: !(Maybe Text)
  , mhLicense     :: !(Maybe License)
  , mhMaintainer  :: !(Maybe Text)
  , mhStability   :: !(Maybe Text)
  , mhPortability :: !(Maybe Text)
  , mhExtra       :: ![(FieldName, Text)]
    -- ^ Fields upstream parses off the front and then discards, in
    -- source order.  We keep them: dropping a field the author bothered
    -- to write is a silent loss, and showing it cannot make our prose
    -- disagree with Hackage's, because Hackage does not render it as
    -- prose either.  @Module@ is not among them \x2014 see 'parseModuleHeader'.
  , mhProse       :: !(Maybe DocText)
    -- ^ Everything after the header block, verbatim, for the markup
    -- parser.  'Nothing' when the description is nothing but fields.
  }
  deriving stock (Show, Eq)

-- | Split a module description into its header fields and its prose.
--
-- The grammar, from upstream's own description of it:
--
-- > [spaces1][field name][spaces] ":"
-- >    [text]"\n" ([spaces2][space][text]"\n" | [spaces]"\n")*
--
-- A field's value runs to the first non-space character indented no
-- further than the first field's key, so blank lines and continuation
-- lines both stay inside the value.  Field parsing stops at the first
-- thing that is not a @name :@ line, and everything from there on is
-- prose, handed back untouched for the markup parser.
--
-- Two consequences worth knowing, both upstream's:
--
-- * A field name is alphabetic characters and hyphens only, so
--   @Note that x: \x2026@ stays prose, while a lone @Example:@ opening the
--   prose is swallowed as a valueless field.  Hackage loses that word
--   too; matching it is why this is a port and not a fresh design.
-- * @Module@ is parsed and then dropped.  Upstream has no slot for it,
--   and a row repeating the page's own title earns nothing.
parseModuleHeader :: DocText -> ModuleHeader
parseModuleHeader (DocText raw) = classify parsed (restore rest)
  where
    input = annotate raw

    (parsed, rest) = case dropSpaces input of
      []             -> ([], input)
      (col, _) : _   -> case fieldsFrom col (dropSpaces input) of
        -- Nothing parsed: the leading whitespace we skipped past is
        -- part of the prose, so hand back the original input.
        ([],  _)     -> ([], input)
        (kvs, after) -> (kvs, after)

-- Field parsing ------------------------------------------------------

-- | The input, each character paired with its zero-based column.  A tab
-- counts as one column, as it does upstream.
type Input = [(Int, Char)]

annotate :: Text -> Input
annotate = go 0 . Text.unpack
  where
    go _   []       = []
    go col (c : cs)
      | c == '\n'   = (col, c) : go 0 cs
      | otherwise   = (col, c) : go (col + 1) cs

restore :: Input -> Text
restore = Text.pack . map snd

dropSpaces :: Input -> Input
dropSpaces = dropWhile (isSpace . snd)

-- | Every field of the block, and whatever follows it.
fieldsFrom :: Int -> Input -> ([(FieldName, Text)], Input)
fieldsFrom startCol = go
  where
    go inp = case field startCol inp of
      Nothing         -> ([], inp)
      Just (kv, more) -> let (kvs, end) = go more in (kv : kvs, end)

-- | One @name : value@ field, or 'Nothing' when the input does not
-- start with one \x2014 which is how the header block ends.
field :: Int -> Input -> Maybe ((FieldName, Text), Input)
field startCol inp = case span (isNameChar . snd) inp of
  ([],   _)     -> Nothing
  (name, afterName) -> case dropSpaces afterName of
    (_, ':') : afterColon ->
      let (value, end) = span (indentedPast startCol) afterColon
      in Just ((FieldName (restore name), Text.strip (restore value)), end)
    _ -> Nothing
  where
    isNameChar c = isAlpha c || c == '-'

-- | A field's value continues over anything blank, and over anything
-- indented further than the key that started the block.
indentedPast :: Int -> (Int, Char) -> Bool
indentedPast startCol (col, c) = isSpace c || col > startCol

-- Classification -----------------------------------------------------

-- | Sort the parsed fields into upstream's slots, keeping the rest.
classify :: [(FieldName, Text)] -> Text -> ModuleHeader
classify fields leftover = ModuleHeader
  { mhDescription = DocText <$> get "Description"
  , mhCopyright   = get "Copyright"
  , mhLicense     = license
  , mhMaintainer  = get "Maintainer"
  , mhStability   = get "Stability"
  , mhPortability = get "Portability"
  , mhExtra       = [ kv | kv@(k, _) <- fields, k `notElem` knownKeys ]
  , mhProse       = if Text.all isSpace leftover
                      then Nothing
                      else Just (DocText leftover)
  }
  where
    -- First occurrence wins, as upstream's lookup does.
    get k = lookup (FieldName k) fields

    license =
          Spdx        <$> get "SPDX-License-Identifier"
      <|> LicenseText <$> get "License"
      <|> LicenseText <$> get "Licence"

-- | The keys upstream reads, plus @Module@, which it reads and drops.
-- Anything else lands in 'mhExtra'.
knownKeys :: [FieldName]
knownKeys = map FieldName
  [ "Module", "Description", "Copyright", "License", "Licence"
  , "SPDX-License-Identifier", "Maintainer", "Stability", "Portability"
  ]
