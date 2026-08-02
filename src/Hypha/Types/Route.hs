{-# LANGUAGE OverloadedStrings #-}
-- | The browser's URL space, built in one place.
--
-- Every page path is @\/pkg\/COMPONENT[\/MODULE[\/SYMBOL]]@, and every
-- segment of it is a name we do not control: Haskell symbols are allowed
-- to contain @#@, @\/@ and @?@, all of which change what a URL /means/
-- rather than merely how it looks.  Concatenating them raw is how
-- @unpackCString#@ came to link to its module page instead of its symbol
-- card — the browser dropped everything from the @#@ on and asked for a
-- path that happened to exist, so the failure was silent — and how
-- @System.FilePath.\<\/\>@ came to 404, its @\/@ having introduced a
-- fourth path segment into a three-capture route.
--
-- So no caller builds a path by concatenation.  'hrefFrom' takes decoded
-- segments and percent-encodes each one; the typed builders name the
-- shapes the site actually has, so a caller holding domain values does
-- not have to unwrap them itself.
module Hypha.Types.Route
  ( hrefFrom
  , packageHref
  , componentHref
  , moduleHref
  , symbolHref
  ) where

import Data.ByteString.Builder qualified as Builder
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Data.ByteString.Lazy qualified as BL
import Network.HTTP.Types.URI (encodePathSegments)

import Hypha.Types.ComponentName (ComponentKey (..))
import Hypha.Types.PackageId (PackageName (..))
import Hypha.Types.SymbolPath (ModulePath (..), SymbolName (..))

-- | An absolute path built from decoded segments, each percent-encoded.
--
-- Unreserved characters survive, so @\/pkg\/containers\/Data.Map@ stays
-- readable and @hypha:exe:hypha@ keeps its colons; only the characters
-- that would change the parse are escaped.
hrefFrom :: [Text] -> Text
hrefFrom =
  Text.decodeUtf8 . BL.toStrict . Builder.toLazyByteString . encodePathSegments

packageHref :: PackageName -> Text
packageHref p = hrefFrom ["pkg", unPackageName p]

componentHref :: ComponentKey -> Text
componentHref c = hrefFrom ["pkg", unComponentKey c]

moduleHref :: ComponentKey -> ModulePath -> Text
moduleHref c m = hrefFrom ["pkg", unComponentKey c, unModulePath m]

symbolHref :: ComponentKey -> ModulePath -> SymbolName -> Text
symbolHref c m n =
  hrefFrom ["pkg", unComponentKey c, unModulePath m, unSymbolName n]
