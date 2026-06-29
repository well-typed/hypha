{-# LANGUAGE OverloadedStrings #-}
-- | Extract exposed module names from a package source directory by
--   reading its @.cabal@ file.
--
--   This module provides a light, text-based parser for the
--   @exposed-modules@ field in Cabal files.  The strategy is:
--
--   1. Find a line starting with @exposed-modules:@ (or @exposed_modules:@).
--   2. Collect subsequent non-blank lines until a blank line.
--   3. From the collected lines, extract anything that looks like a Haskell
--      module name (dot-separated uppercase-started identifiers).
module Hypha.Source.Modules
  ( -- * Lookup
    findCabalFile
  , getExposedModules
    -- * Parsing
  , parseExposedModules
  ) where

import Data.List (isSuffixOf)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import System.Directory (doesFileExist, listDirectory)
import System.FilePath ((</>))

-- | Find the single @.cabal@ file inside a package source directory.
--   Returns 'Nothing' if no @.cabal@ file exists or if there are multiple
--   (which should not happen for a well-formed package).
findCabalFile :: FilePath -> IO (Maybe FilePath)
findCabalFile root = do
  entries <- listDirectory root
  case filter (\e -> ".cabal" `isSuffixOf` e) entries of
    [f] -> do
      let fp = root </> f
      ok <- doesFileExist fp
      pure (if ok then Just fp else Nothing)
    _   -> pure Nothing

-- | Get exposed modules from a package source directory.
--   Looks for a @.cabal@ file and parses its @exposed-modules@ stanza.
--   Returns an empty list if the file cannot be found or parsed.
getExposedModules :: FilePath -> IO [Text]
getExposedModules root = do
  mCabal <- findCabalFile root
  case mCabal of
    Nothing  -> pure []
    Just fp  -> do
      content <- TIO.readFile fp
      pure (parseExposedModules content)

-- | Parse the @exposed-modules@ field from the text of a @.cabal@ file.
--
--   We find the first occurrence of @exposed-modules:@ (or @exposed_modules:@),
--   collect all subsequent non-blank lines until a blank line, then extract
--   Haskell module names from all those lines.
--
--   A "Haskell module name" is a dot-separated sequence of identifiers
--   where the first character is uppercase.  This heuristic reliably matches
--   module names in all real-world @.cabal@ files while rejecting field
--   headers like @build-depends@, version constraints, etc.
parseExposedModules :: Text -> [Text]
parseExposedModules content =
  let ls = Text.lines content
  in case findExposedModulesSection ls of
       Nothing  -> []
       Just raw -> collectModuleNames raw

-- | Find the @exposed-modules@ header line and collect all following
--   non-blank lines until a blank line.
findExposedModulesSection :: [Text] -> Maybe [Text]
findExposedModulesSection []     = Nothing
findExposedModulesSection (l:ls)
  | isExposedModulesHeader l = Just (l : takeWhile (not . Text.null . Text.strip) ls)
  | otherwise                = findExposedModulesSection ls

-- | Does this line start an @exposed-modules@ stanza?
isExposedModulesHeader :: Text -> Bool
isExposedModulesHeader t =
  let stripped = Text.strip t
  in "exposed-modules:" `Text.isPrefixOf` stripped
     || "exposed_modules:" `Text.isPrefixOf` stripped

-- | Collect Haskell module names from a list of raw lines.
--
--   Strategy: take all text, split into comma-separated segments, and keep
--   segments that look like module names after stripping parenthesised
--   sub-lists.
collectModuleNames :: [Text] -> [Text]
collectModuleNames [] = []
collectModuleNames lines'@(_:xs) =
  let -- Strip the field header prefix from the first line
      firstText = stripHeader lines'
      -- For all lines, strip leading whitespace and commas, then split
      -- each line on commas
      allSegments = concatMap extractSegments (firstText : map cleanContinuation xs)
      -- Clean each segment (strip paren sub-lists) and keep module names
  in mapMaybe cleanAndCheckModuleName (map Text.strip allSegments)
  where
    stripHeader :: [Text] -> Text
    stripHeader []     = ""
    stripHeader (hd:_) =
      -- Everything after the first colon
      let after = Text.drop 1 (Text.dropWhile (/= ':') hd)
      in Text.strip after

    -- Clean a continuation line: just strip leading whitespace/commas
    cleanContinuation :: Text -> Text
    cleanContinuation = Text.strip . Text.dropWhile (== ',')

    -- Split a line on commas, returning non-empty segments
    extractSegments :: Text -> [Text]
    extractSegments t = filter (not . Text.null . Text.strip) (Text.splitOn "," t)

    -- Clean a single segment and check if it's a module name:
    -- drop parenthesised sub-lists then check the leading identifier
    cleanAndCheckModuleName :: Text -> Maybe Text
    cleanAndCheckModuleName t =
      let -- Drop parenthesised suffix: "Foo(Bar, Baz)" -> "Foo"
          noParens = Text.takeWhile (/= '(') t
          trimmed = Text.strip noParens
      in if looksLikeModuleName trimmed
           then Just trimmed
           else Nothing

-- | Heuristic: a string looks like a Haskell module name if it starts with
--   an uppercase letter and consists only of alphanumeric, dot, underscore,
--   and single-quote characters.
looksLikeModuleName :: Text -> Bool
looksLikeModuleName t
  | Text.null t = False
  | otherwise   =
      let first = Text.head t
      in first >= 'A' && first <= 'Z'
         && Text.all isValidModuleChar (Text.tail t)

-- | Valid character inside a module name.
isValidModuleChar :: Char -> Bool
isValidModuleChar c =
     (c >= 'a' && c <= 'z')
  || (c >= 'A' && c <= 'Z')
  || (c >= '0' && c <= '9')
  || c == '.'
  || c == '_'
  || c == '\''
