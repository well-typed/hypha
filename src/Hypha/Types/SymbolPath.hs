{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Types.SymbolPath
  ( SymbolPath (..)
  , ModulePath (..)
  , mainModulePath
  , SymbolName (..)
  , Signature (..)
  , ParseError (..)
  , parseSymbolPath
  , renderSymbolPath
  ) where

import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as Text

import Hypha.Types.PackageId
  ( PackageName (..), Version (..), PackageRef (..), parsePackageRef )

newtype ModulePath = ModulePath { unModulePath :: Text }
  deriving stock (Show, Eq, Ord)

-- | The module an executable's entry point is called unless it says
-- otherwise: GHC's default, and what a source file with no
-- @module … where@ header parses as.
--
-- One definition, because two places need it and they must agree: the
-- parser names a header-less module this, and a stanza's @main-is@ is
-- /expected/ to declare it.  A second literal would be a second answer.
mainModulePath :: ModulePath
mainModulePath = ModulePath "Main"

newtype SymbolName = SymbolName { unSymbolName :: Text }
  deriving stock (Show, Eq, Ord)

-- | A rendered Haskell type signature (@insertWith :: Ord k => …@).
--
-- The search index stores these and the UI renders them; between those
-- two points nothing should be able to mistake one for a module path or
-- a symbol name, which is what a bare 'Text' invited.
newtype Signature = Signature { unSignature :: Text }
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
  | InvalidModule !Text
  | InvalidSymbol !Text
  | ModuleSegmentNotUppercase !Text
  deriving stock (Show, Eq)

-- | A module path consists of dot-separated segments, each of which must
-- start with an uppercase ASCII letter (Haskell module convention).
validateModulePath :: Text -> Either ParseError ModulePath
validateModulePath m
  | Text.null m = Left (InvalidModule m)
  | otherwise =
      let segs = Text.splitOn "." m
      in case foldr validateSeg (Right ()) segs of
           Right () -> Right (ModulePath m)
           Left err -> Left err
  where
    validateSeg :: Text -> Either ParseError () -> Either ParseError ()
    validateSeg _ err@(Left _) = err
    validateSeg seg (Right ())
      | Text.null seg = Left (InvalidModule seg)
      | otherwise = case Text.uncons seg of
          Just (c, _) | c >= 'A' && c <= 'Z' -> Right ()
          _ -> Left (ModuleSegmentNotUppercase seg)

-- | Parse @pkg[-ver][/Mod[.Path]][/sym]@.  The version segment uses
-- the Haskell convention (hyphen separator, all digits and dots) —
-- the @\@ver@ syntax is no longer accepted.
parseSymbolPath :: Text -> Either ParseError SymbolPath
parseSymbolPath t
  | Text.null t = Left EmptyInput
  | otherwise =
      let segs    = Text.splitOn "/" t
          (pkgSeg, mModSeg, mSymSeg) = case segs of
            []              -> ("", Nothing, Nothing)
            [p]             -> (p, Nothing, Nothing)
            [p, m]          -> (p, Just m, Nothing)
            (p : m : s : _) -> (p, Just m, Just s)
          PackageRef pkg mv = parsePackageRef pkgSeg
      in do
        when (Text.null (unPackageName pkg))
             (Left EmptyPackageSegment)
        when (Text.any (`elem` ("/@" :: String)) (unPackageName pkg))
             (Left (InvalidPackageName (unPackageName pkg)))
        mm <- case mModSeg of
                Nothing -> Right Nothing
                Just m  -> if Text.null m
                             then if mSymSeg /= Nothing
                                    then Left SymbolWithoutModule
                                    else Left (InvalidModule m)
                             else Just <$> validateModulePath m
        ms <- case mSymSeg of
                Nothing -> Right Nothing
                Just s  -> if Text.null s
                             then Left (InvalidSymbol s)
                             else Right (Just (SymbolName s))
        case (mm, ms) of
          (Nothing, Just _) -> Left SymbolWithoutModule
          _                 -> Right (SymbolPath pkg mv mm ms)

renderSymbolPath :: SymbolPath -> Text
renderSymbolPath (SymbolPath (PackageName p) mv mm ms) =
     p
  <> maybe "" (\(Version v)    -> "-" <> v) mv
  <> maybe "" (\(ModulePath m) -> "/" <> m) mm
  <> maybe "" (\(SymbolName s) -> "/" <> s) ms
