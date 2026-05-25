{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Project.Overrides
  ( -- * Types
    OverrideError (..)
  , renderOverrideError
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

-- | User-facing renderer for 'OverrideError'.  Only call this at the
-- wire boundary (CLI parse-error message, error envelope) — never
-- inside an error constructor.
renderOverrideError :: OverrideError -> Text
renderOverrideError = \case
  MissingEquals raw -> "expected PKG=VER override (got: " <> raw <> ")"
  EmptyPackageName  -> "empty package name in --package-override"
  EmptyVersion      -> "empty version in --package-override"

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
