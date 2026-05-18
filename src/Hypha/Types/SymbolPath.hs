{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Types.SymbolPath
  ( SymbolPath (..)
  , ModulePath (..)
  , SymbolName (..)
  , ParseError (..)
  , parseSymbolPath
  , renderSymbolPath
  ) where

import Data.Text (Text)
import qualified Data.Text as Text

import Hypha.Types.PackageId (PackageName (..), Version (..), parsePackageName, parseVersion)

newtype ModulePath = ModulePath { unModulePath :: Text }
  deriving stock (Show, Eq, Ord)

newtype SymbolName = SymbolName { unSymbolName :: Text }
  deriving stock (Show, Eq, Ord)

data SymbolPath = SymbolPath
  { spPackage :: !PackageName
  , spVersion :: !(Maybe Version)
  , spModule  :: !(Maybe ModulePath)
  , spSymbol  :: !(Maybe SymbolName)
  }
  deriving stock (Show, Eq, Ord)

data ParseError
  = EmptyInput
  | EmptyPackageSegment
  | SymbolWithoutModule
  | InvalidPackageName !Text
  | InvalidVersion !Text
  | InvalidModule !Text
  | InvalidSymbol !Text
  deriving stock (Show, Eq)

-- | Parse @pkg[@ver][/Mod[.Path]][/sym]@.
parseSymbolPath :: Text -> Either ParseError SymbolPath
parseSymbolPath t
  | Text.null t = Left EmptyInput
  | otherwise =
      let segs    = Text.splitOn "/" t
          (pkgSeg, mModSeg, mSymSeg) = case segs of
            []                 -> ("", Nothing, Nothing)
            [p]                -> (p, Nothing, Nothing)
            [p, m]             -> (p, Just m, Nothing)
            (p : m : s : _)    -> (p, Just m, Just s)
          (pkgPart, mVerPart) = case Text.splitOn "@" pkgSeg of
            [p]    -> (p, Nothing)
            [p, v] -> (p, Just v)
            (p:_)  -> (p, Nothing)
            []     -> ("", Nothing)
      in do
        pkg <- maybe (Left (InvalidPackageName pkgPart)) Right (parsePackageName pkgPart)
        mv  <- case mVerPart of
                 Nothing -> Right Nothing
                 Just v  -> maybe (Left (InvalidVersion v)) (Right . Just) (parseVersion v)
        mm  <- case mModSeg of
                 Nothing -> Right Nothing
                 Just m  -> if Text.null m
                                then if mSymSeg /= Nothing
                                       then Left SymbolWithoutModule
                                       else Left (InvalidModule m)
                                else Right (Just (ModulePath m))
        ms  <- case mSymSeg of
                 Nothing -> Right Nothing
                 Just s  -> if Text.null s then Left (InvalidSymbol s) else Right (Just (SymbolName s))
        case (mm, ms) of
          (Nothing, Just _) -> Left SymbolWithoutModule
          _                 -> Right (SymbolPath pkg mv mm ms)

renderSymbolPath :: SymbolPath -> Text
renderSymbolPath (SymbolPath (PackageName p) mv mm ms) =
     p
  <> maybe "" (\(Version v)        -> "@" <> v) mv
  <> maybe "" (\(ModulePath m)     -> "/" <> m) mm
  <> maybe "" (\(SymbolName s)     -> "/" <> s) ms
