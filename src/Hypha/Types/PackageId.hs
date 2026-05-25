{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hypha.Types.PackageId
  ( PackageName (..)
  , Version (..)
  , PackageId (..)
  , PackageRef (..)
  , parsePackageName
  , parseVersion
  , parsePackageRef
  , renderPackageId
  , renderPackageRef
  ) where

import Data.Char (isDigit, isHexDigit)
import Data.Text (Text)
import qualified Data.Text as Text

newtype PackageName = PackageName { unPackageName :: Text }
  deriving stock   (Show, Eq, Ord)
  deriving newtype (Read)

newtype Version = Version { unVersion :: Text }
  deriving stock   (Show, Eq, Ord)
  deriving newtype (Read)

data PackageId = PackageId
  { pkgName    :: !PackageName
  , pkgVersion :: !Version
  }
  deriving stock (Show, Eq, Ord)

parsePackageName :: Text -> Maybe PackageName
parsePackageName t
  | Text.null t                              = Nothing
  | Text.any (\c -> c == '/' || c == '@') t  = Nothing
  | otherwise                                = Just (PackageName t)

parseVersion :: Text -> Maybe Version
parseVersion t
  | Text.null t                              = Nothing
  | Text.any (\c -> c == '/' || c == '@') t  = Nothing
  | otherwise                                = Just (Version t)

renderPackageId :: PackageId -> Text
renderPackageId (PackageId (PackageName n) (Version v)) = n <> Text.pack "-" <> v

-- | User-supplied reference to a package.  The version is optional: a
-- bare 'PackageName' means "whatever the plan / store / Hackage thinks
-- is current", while a 'Just' version pins resolution to that release.
data PackageRef = PackageRef
  { refName    :: !PackageName
  , refVersion :: !(Maybe Version)
  }
  deriving stock (Show, Eq, Ord)

-- | Render a 'PackageRef' back to its canonical @pkg-version@ form
-- (or just @pkg@ when no version is pinned).
renderPackageRef :: PackageRef -> Text
renderPackageRef (PackageRef (PackageName n) mv) = case mv of
  Nothing          -> n
  Just (Version v) -> n <> Text.pack "-" <> v

-- | Parse a user-supplied package reference.  Accepts:
--
--   * @async@                    — bare name, no version pin;
--   * @async-2.2.6@              — Haskell convention, name + version;
--   * @async-2.2.6-2cf97...@     — cabal store entry (trailing hash
--                                  recognised by length >= 8 and
--                                  hex-only content, then stripped
--                                  before re-parsing the remainder).
--
-- The parser never fails: any input that does not match the above
-- shapes is treated as a bare package name.  Callers that need a
-- pinned version should look at 'refVersion'.
parsePackageRef :: Text -> PackageRef
parsePackageRef raw =
  case splitOffVersion (stripStoreHash raw) of
    (n, mv) -> PackageRef (PackageName n) mv

-- | Drop a trailing @-<hash>@ segment when present.  Recognised by
-- length >= 8 and being entirely hex digits — matches the shape cabal
-- uses for its store entries.
stripStoreHash :: Text -> Text
stripStoreHash t = case Text.breakOnEnd (Text.pack "-") t of
  (prefix, suffix)
    | not (Text.null prefix)
    , looksLikeHash suffix
    -> Text.init prefix
  _ -> t

-- | Peel off a trailing @-<version>@ segment when the suffix looks
-- like a cabal version literal (digits and dots only).
splitOffVersion :: Text -> (Text, Maybe Version)
splitOffVersion t = case Text.breakOnEnd (Text.pack "-") t of
  (prefix, suffix)
    | not (Text.null prefix)
    , looksLikeVersion suffix
    -> (Text.init prefix, Just (Version suffix))
  _ -> (t, Nothing)

looksLikeVersion :: Text -> Bool
looksLikeVersion v =
  not (Text.null v) && Text.all (\c -> isDigit c || c == '.') v

looksLikeHash :: Text -> Bool
looksLikeHash v = Text.length v >= 8 && Text.all isHexDigit v
