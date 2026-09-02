{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hypha.Types.PackageId
  ( PackageName (..)
  , Version (..)
  , UnitId (..)
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

-- | cabal's identifier for one /configuration/ of one component:
-- @attoparsec-0.14.4-88042e465d110de0…@.
--
-- The hash covers the compiler, the resolved dependency unit-ids and the
-- flag assignment, which is exactly the set of inputs that decides what
-- a module of that component exports.  Two projects that agree on all of
-- it get the same id and can share indexed rows; two that do not, cannot.
--
-- Read from @plan.json@ rather than computed: cabal already did the work,
-- and a second definition of "same configuration" would be a second
-- answer to drift from.  A boot package arrives with a ghc-pkg-style id
-- (@base-4.20.2.0@) carrying no configuration hash, so for those the
-- identity degenerates to the version — acceptable because two compilers
-- practically never ship one @base@ version, but it is an assumption.
newtype UnitId = UnitId { unUnitId :: Text }
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
