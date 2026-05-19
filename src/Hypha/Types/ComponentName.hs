{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
-- | A cabal-style component reference: a package name with an optional
-- sub-library qualifier.
--
-- @nike@ refers to the main library; @nike:lib-breakdown@ refers to the
-- @lib-breakdown@ sub-library.  The composite form is what cabal-install
-- uses on the command line, and what we put in the @pkg@ column of the
-- SQLite search cache so sublibs don't need a schema migration.
module Hypha.Types.ComponentName
  ( ComponentName (..)
  , parseComponentName
  , renderComponentName
  ) where

import Data.Text (Text)
import qualified Data.Text as Text

import Hypha.Types.PackageId (PackageName (..))

-- | Reference to a single library component in the build plan.
data ComponentName = ComponentName
  { cnPackage :: !PackageName
  , cnSublib  :: !(Maybe Text)
    -- ^ 'Nothing' for the main library; 'Just' for a sub-library.
  }
  deriving stock (Show, Eq, Ord)

-- | Split a textual reference on the first @:@.  An empty sub-library
-- suffix (e.g. @"pkg:"@) collapses to 'Nothing' so it round-trips with
-- the plain @"pkg"@ form.
parseComponentName :: Text -> ComponentName
parseComponentName raw =
  case Text.breakOn ":" raw of
    (pkg, rest)
      | Text.null rest -> ComponentName (PackageName pkg) Nothing
      | otherwise      ->
          let sublib = Text.drop 1 rest
          in ComponentName (PackageName pkg)
               (if Text.null sublib then Nothing else Just sublib)

-- | Inverse of 'parseComponentName'.
renderComponentName :: ComponentName -> Text
renderComponentName (ComponentName (PackageName p) Nothing)  = p
renderComponentName (ComponentName (PackageName p) (Just s)) = p <> ":" <> s
