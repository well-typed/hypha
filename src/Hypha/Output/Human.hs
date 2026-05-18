{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Output.Human
  ( renderSymbolCard
  , renderHaddock
  , renderSignature
  ) where

import qualified Data.Text as Text
import Data.Text (Text)
import qualified Documentation.Haddock.Parser as HP
import qualified Documentation.Haddock.Types  as HT
import qualified Skylighting as Sky
import Prettyprinter (Doc, pretty, vsep, indent, (<+>), emptyDoc, line)
import qualified Prettyprinter as PP
import Prettyprinter.Render.Terminal (AnsiStyle, color, Color (..), bold, italicized)

-- | Render a symbol card as an ANSI-styled document.
renderSymbolCard
  :: Text  -- ^ name
  -> Text  -- ^ kind
  -> Text  -- ^ signature
  -> Text  -- ^ haddock_raw
  -> Text  -- ^ source path
  -> Int   -- ^ source line
  -> Doc AnsiStyle
renderSymbolCard name kind sig hRaw srcPath srcLine = vsep
  [ PP.annotate bold (pretty name) <+> PP.annotate (color Cyan) (pretty kind)
  , indent 2 (renderSignature sig)
  , emptyDoc
  , indent 2 (renderHaddock hRaw)
  , emptyDoc
  , PP.annotate (color Yellow)
      (pretty srcPath <> ":" <> pretty (Text.pack (show srcLine)))
  ]

-- | Render a Haskell signature with syntax highlighting.
renderSignature :: Text -> Doc AnsiStyle
renderSignature t =
  case Sky.lookupSyntax "Haskell" Sky.defaultSyntaxMap of
    Nothing -> pretty t
    Just syn ->
      case Sky.tokenize config syn t of
        Left  _        -> pretty t
        Right srcLines -> pretty (formatSourceLines srcLines)
  where
    config = Sky.TokenizerConfig
      { Sky.syntaxMap   = Sky.defaultSyntaxMap
      , Sky.traceOutput = False
      }

-- | Format Skylighting source lines as ANSI-colored text.
formatSourceLines :: [Sky.SourceLine] -> Text
formatSourceLines = Text.intercalate "\n" . map formatSourceLine

-- | Format a single source line.
formatSourceLine :: Sky.SourceLine -> Text
formatSourceLine = Text.concat . map formatToken

-- | Format a single token.
formatToken :: Sky.Token -> Text
formatToken (_tokType, txt) = txt
  -- Note: Full ANSI formatting per token type would require
  -- mapping Skylighting token types to ANSI colors.
  -- For now, we pass through the raw text.
  -- Post-MVP: use Skylighting.Format.ANSI for proper coloring.

-- | Parse a Haddock DocH string and render as ANSI-styled document.
renderHaddock :: Text -> Doc AnsiStyle
renderHaddock src =
  let doc = HP.parseString (Text.unpack src)
  in fromDocH doc

-- | Convert a Haddock DocH AST to an ANSI-styled document.
fromDocH :: HT.DocH a b -> Doc AnsiStyle
fromDocH = \case
  HT.DocEmpty        -> emptyDoc
  HT.DocAppend x y   -> fromDocH x <> fromDocH y
  HT.DocString s     -> pretty (Text.pack s)
  HT.DocParagraph x  -> fromDocH x <> line <> line
  HT.DocIdentifier _ -> PP.annotate (color Magenta) (pretty ("\x27E8id\x27E9" :: Text))
  HT.DocIdentifierUnchecked _ -> pretty ("\x27E8id\x27E9" :: Text)
  HT.DocModule m     -> PP.annotate italicized (pretty (Text.pack (HT.modLinkName m)))
  HT.DocEmphasis x   -> PP.annotate italicized (fromDocH x)
  HT.DocBold x       -> PP.annotate bold (fromDocH x)
  HT.DocMonospaced x -> PP.annotate (color Cyan) (fromDocH x)
  HT.DocCodeBlock x  -> indent 4 (fromDocH x)
  HT.DocHyperlink _  -> PP.annotate (color Blue) (pretty ("\x27E8link\x27E9" :: Text))
  HT.DocPic _        -> pretty ("\x27E8pic\x27E9" :: Text)
  HT.DocAName _      -> emptyDoc
  HT.DocProperty _   -> emptyDoc
  HT.DocExamples _   -> emptyDoc
  HT.DocHeader h     -> PP.annotate bold (fromDocH (HT.headerTitle h)) <> line
  HT.DocTable _      -> pretty ("\x27E8table\x27E9" :: Text)
  HT.DocUnorderedList xs -> vsep (map (\x -> pretty ("\x2022 " :: Text) <> fromDocH x) xs)
  HT.DocOrderedList xs -> vsep (zipWith (\i x -> pretty (Text.pack (show (i :: Int) <> ". ")) <> fromDocH x) [1..] (map snd xs))
  HT.DocDefList xs   -> vsep [ fromDocH k <> pretty (":" :: Text) <+> fromDocH v | (k,v) <- xs ]
  HT.DocMathInline _ -> pretty ("\x27E8math\x27E9" :: Text)
  HT.DocMathDisplay _ -> pretty ("\x27E8math\x27E9" :: Text)
  HT.DocWarning x    -> PP.annotate (color Yellow) (fromDocH x)
