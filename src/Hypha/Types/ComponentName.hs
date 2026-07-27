{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
-- | A cabal-style component reference: a package name with an
-- optional sub-library or executable qualifier.
--
-- Encoded form:
--
-- * @hypha@                 — main library.
-- * @hypha:lib-breakdown@   — sub-library @lib-breakdown@.
-- * @hypha:exe:hypha-cli@    — executable @hypha-cli@.
--
-- The composite form is what cabal-install uses on the command line,
-- and what we put in the @pkg@ column of the SQLite search cache so
-- sublibs and executables don't need a schema migration.
module Hypha.Types.ComponentName
  ( ComponentName (..)
  , parseComponentName
  , renderComponentName
  , ComponentKey (..)
  , componentKeyOf
  , parseComponentKey
  ) where

import Data.Text (Text)
import qualified Data.Text as Text

import Hypha.Project.Components (ComponentKind (..))
import Hypha.Types.PackageId    (PackageName (..))

-- | Reference to a single library or executable component.
data ComponentName = ComponentName
  { cnPackage :: !PackageName
  , cnKind    :: !ComponentKind
  }
  deriving stock (Show, Eq, Ord)

-- | Split a textual reference on @:@.  The grammar is:
--
-- * @pkg@               → 'MainLib'
-- * @pkg:exe:name@      → 'Exe name'
-- * @pkg:name@          → 'SubLib name'
--
-- Empty suffixes (@pkg:@, @pkg:exe:@) collapse to 'MainLib' so they
-- round-trip cleanly with the bare @pkg@ form.
parseComponentName :: Text -> ComponentName
parseComponentName raw =
  case Text.breakOn ":" raw of
    (pkg, rest)
      | Text.null rest -> ComponentName (PackageName pkg) MainLib
      | otherwise      ->
          let afterColon = Text.drop 1 rest
          in case Text.stripPrefix "exe:" afterColon of
               Just exeName
                 | Text.null exeName ->
                     ComponentName (PackageName pkg) MainLib
                 | otherwise         ->
                     ComponentName (PackageName pkg) (Exe exeName)
               Nothing
                 | Text.null afterColon ->
                     ComponentName (PackageName pkg) MainLib
                 | otherwise            ->
                     ComponentName (PackageName pkg) (SubLib afterColon)

-- | Inverse of 'parseComponentName'.
renderComponentName :: ComponentName -> Text
renderComponentName (ComponentName (PackageName p) MainLib)    = p
renderComponentName (ComponentName (PackageName p) (SubLib s)) = p <> ":" <> s
renderComponentName (ComponentName (PackageName p) (Exe    s)) = p <> ":exe:" <> s

-- | The rendered component reference, as it appears in the @pkg@ column
-- of the search cache and in @\/pkg\/…@ URLs.
--
-- A newtype because the encoding matters: @containers@,
-- @hypha:hypha-internal@ and @hypha:exe:hypha-mcp@ are three shapes of
-- one thing, and code that takes a bare 'Text' here cannot say whether
-- it holds a key, a package name, or a module path.
newtype ComponentKey = ComponentKey { unComponentKey :: Text }
  deriving stock (Show, Eq, Ord)

-- | Build the key for a package's component.
componentKeyOf :: PackageName -> ComponentKind -> ComponentKey
componentKeyOf pkg kind = ComponentKey (renderComponentName (ComponentName pkg kind))

-- | Strict inverse of 'componentKeyOf'.
--
-- Distinct from 'parseComponentName', which is deliberately lenient
-- because it reads user-supplied URLs and must always produce something.
-- This one validates: an empty segment or a segment count the encoding
-- cannot produce yields 'Nothing' rather than a component reference
-- nobody meant.
--
-- Note a sub-library may legitimately be called @exe@: two segments are
-- always a sub-library, and only a three-segment key with @exe@ in the
-- middle is an executable, so @pkg:exe@ and @pkg:exe:name@ stay
-- distinguishable.
parseComponentKey :: Text -> Maybe (Text, ComponentKind)
parseComponentKey raw = case Text.splitOn ":" raw of
  [pkg]              | nonEmpty [pkg]       -> Just (pkg, MainLib)
  [pkg, sub]         | nonEmpty [pkg, sub]  -> Just (pkg, SubLib sub)
  [pkg, "exe", name] | nonEmpty [pkg, name] -> Just (pkg, Exe name)
  _                                         -> Nothing
  where
    nonEmpty = all (not . Text.null)
