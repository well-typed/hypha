{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Project.Overrides
  ( -- * Types
    OverrideError (..)
    -- * Parsing
  , parsePackageOverride
  ) where

import Data.Text (Text)
import qualified Data.Text as Text

import Hypha.Types.PackageId (PackageName (..), Version (..))
import Hypha.Types.BuildPlan (PackageOverride (..))

-- | Errors that can occur when parsing a @PKG=VER@ override string.
data OverrideError
  = MissingEquals !Text
    -- ^ The override string does not contain @=@.
  | EmptyPackageName
    -- ^ The package name part (before @=@) is empty.
  | EmptyVersion
    -- ^ The version part (after @=@) is empty.
  deriving stock (Show, Eq)

-- | Parse a @PKG=VER@ override string.
--
--   >>> parsePackageOverride "async=2.2.6"
--   Right (PackageOverride {poName = PackageName \"async\", poVersion = Version \"2.2.6\"})
--
--   >>> parsePackageOverride "bad-string"
--   Left (MissingEquals \"bad-string\")
parsePackageOverride :: Text -> Either OverrideError PackageOverride
parsePackageOverride t =
  case Text.breakOn "=" t of
    (_, rest) | Text.null rest -> Left (MissingEquals t)
    (namePart, verPart) ->
      let name = Text.strip namePart
          ver  = Text.strip (Text.drop 1 verPart) -- drop the '='
      in if Text.null name
         then Left EmptyPackageName
         else if Text.null ver
              then Left EmptyVersion
              else Right (PackageOverride (PackageName name) (Version ver))
