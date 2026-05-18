{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Source.Locate
  ( listExportedSymbols
  , locateSymbolDefinition
  , SourceLocation (..)
    -- * Testing
  , parseExports
  , modulePathToFile
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import System.Directory (doesFileExist)
import System.FilePath ((</>))

import Hypha.BuildEnv.Type   (BuildEnv (..))
import Hypha.Types.PackageId (PackageId (..))

-- | Location of a symbol definition in a source file.
data SourceLocation = SourceLocation
  { slPath :: !FilePath
  , slLine :: !Int
  }
  deriving stock (Show, Eq)

-- | Inspect the package source directory and list candidate exported symbols
-- by scraping the @module ... ( ... ) where@ header naïvely.
--
-- A future improvement (post-MVP) is to use ghc-lib-parser for accurate parsing.
listExportedSymbols :: BuildEnv IO -> PackageId -> Text -> IO [Text]
listExportedSymbols env pid modPath = do
  mDir <- locatePackageSource env pid
  case mDir of
    Nothing -> pure []
    Just d  -> do
      let f = d </> modulePathToFile modPath
      ok <- doesFileExist f
      if not ok
        then pure []
        else parseExports <$> TIO.readFile f

-- | Convert a module path like @Control.Concurrent.Async@ to a file path
-- like @Control/Concurrent/Async.hs@.
modulePathToFile :: Text -> FilePath
modulePathToFile m = Text.unpack (Text.replace "." "/" m) <> ".hs"

-- | Crude header parser.  Returns the comma-separated identifiers in the
-- module's explicit export list.  Designed to be conservative: when the
-- shape of the header is unfamiliar (no export list, missing @module@
-- keyword, exports written after @where@, etc.) it returns @[]@ rather
-- than guessing.
--
-- Specifically we require the @module@ keyword, an open paren that appears
-- before any @where@ token, and a matching close paren that appears before
-- the corresponding @where@.  Constructor lists @Foo(..)@, sub-exports
-- @Foo(Bar,Baz)@ and operators are tolerated but not deeply parsed; only the
-- leading identifier per entry is reported.
parseExports :: Text -> [Text]
parseExports src
  | not hasModuleKw            = []
  | Text.null afterOpenParen   = []
  | not openBeforeWhere        = []
  | Text.null exportListRaw    = []
  | otherwise                  = take 100 normalised
  where
    -- Strip line comments to avoid mistaking '(' inside @--@ for the export list.
    stripped = Text.unlines (map dropLineComment (Text.lines src))
    dropLineComment l = case Text.breakOn "--" l of (a, _) -> a

    -- Find the start of the export list ('(' after "module Foo").
    afterModule    = Text.dropWhile (/= 'm') stripped
    hasModuleKw    = "module" `Text.isPrefixOf` Text.dropWhile (== 'm') afterModule
                    || "module " `Text.isInfixOf` stripped

    -- We split on the first opening paren that appears after "module" and
    -- before "where".
    fromOpenParen = Text.dropWhile (/= '(') stripped
    afterOpenParen = Text.drop 1 fromOpenParen
    openBeforeWhere =
      let idxParen = Text.length stripped - Text.length fromOpenParen
          idxWhere = Text.length stripped
                   - Text.length (Text.dropWhile (\_ -> False) (snd (Text.breakOn "where" stripped)))
      in idxParen < idxWhere

    -- Take a balanced span up to the close paren before "where".  Naive: we
    -- accept up to the first ')' followed (eventually) by "where".  Good
    -- enough for canonical Haskell module headers; falls through to [] if
    -- the header is in an unfamiliar shape.
    exportListRaw =
      let (lhs, _) = Text.breakOn "where" afterOpenParen
          -- drop trailing close paren if present
          trimmed = case Text.breakOnEnd ")" lhs of
                      ("", _) -> ""
                      (a, _)  -> Text.dropEnd 1 a
      in trimmed

    -- Split on top-level commas (we ignore nested parens for sub-exports).
    entries = splitTopLevel exportListRaw

    normalised = [ leading e | e <- entries, not (Text.null (leading e)) ]

    -- The leading identifier of an entry (drop optional 'pattern' / 'type'
    -- modifiers and sub-export parens).
    leading :: Text -> Text
    leading e0 =
      let e = trim e0
          e' = stripPrefixWord "pattern" e
          e'' = stripPrefixWord "type" e'
          ident = Text.takeWhile (\c -> c /= '(' && c /= ',' && c /= ' ') e''
      in ident

    stripPrefixWord :: Text -> Text -> Text
    stripPrefixWord w t = case Text.stripPrefix (w <> " ") t of
      Just rest -> rest
      Nothing   -> t

    splitTopLevel :: Text -> [Text]
    splitTopLevel = go 0 Text.empty
      where
        go _depth acc t = case Text.uncons t of
          Nothing                         -> [acc]
          Just (',', rest) | _depth == 0  -> acc : go 0 Text.empty rest
          Just ('(', rest)                -> go (_depth + 1) (Text.snoc acc '(') rest
          Just (')', rest) | _depth > 0   -> go (_depth - 1) (Text.snoc acc ')') rest
          Just (c, rest)                  -> go _depth (Text.snoc acc c) rest

    trim :: Text -> Text
    trim = Text.dropWhile (`elem` (" \t\n\r" :: String))
         . Text.dropWhileEnd (`elem` (" \t\n\r" :: String))

-- | Find the line where a symbol is defined.
locateSymbolDefinition :: BuildEnv IO -> PackageId -> Text -> Text -> IO (Maybe SourceLocation)
locateSymbolDefinition env pid modPath sym = do
  mDir <- locatePackageSource env pid
  case mDir of
    Nothing -> pure Nothing
    Just d  -> do
      let f = d </> modulePathToFile modPath
      ok <- doesFileExist f
      if not ok
        then pure Nothing
        else do
          ls <- Text.lines <$> TIO.readFile f
          pure $ case [ i | (i, l) <- zip [1 :: Int ..] ls, startsWith sym l ] of
                   (i:_) -> Just (SourceLocation f i)
                   []    -> Nothing
  where
    startsWith name l =
      let trimmed = Text.dropWhile (== ' ') l
      in name `Text.isPrefixOf` trimmed
