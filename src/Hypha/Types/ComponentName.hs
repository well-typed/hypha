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
