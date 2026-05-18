{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hypha.Types.PackageId
  ( PackageName (..)
  , Version (..)
  , PackageId (..)
  , parsePackageName
  , parseVersion
  , renderPackageId
  ) where

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
