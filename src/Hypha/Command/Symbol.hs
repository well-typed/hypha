{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Command.Symbol
  ( SymbolResult (..)
  , compactKeys
  , fullKeys
  , runSymbol
  ) where

import Data.Aeson (Value, object, (.=))
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import System.Directory (doesFileExist)
import System.FilePath ((</>))

import Hypha.BuildEnv.Type (BuildEnv (..))
import Hypha.Error (HyphaError (..))
import Hypha.Output.Outcome (Outcome (..), Related (..))
import Hypha.Source.Extract (SymbolInfo (..), extractSymbolInfo)
import Hypha.Source.Locate (modulePathToFile)
import Hypha.Types.BuildPlan (BuildPlan, lookupPackage)
import Hypha.Types.PackageId (PackageId (..), PackageName (..), Version (..))
import Hypha.Types.SymbolPath
  ( SymbolPath (..), ModulePath (..), SymbolName (..), parseSymbolPath )
import Hypha.Types.Doc (DocText (..))

-- | Result of the @symbol@ command.
data SymbolResult = SymbolResult
  { srName      :: !Text
  , srKind      :: !Text
  , srPackage   :: !Text
  , srVersion   :: !Text
  , srModule    :: !Text
  , srSignature :: !(Maybe Text)
  , srHaddock   :: !(Maybe Text)
  , srSource    :: !(Maybe SourceLoc)
  }
  deriving stock (Show, Eq)

-- | Source location within a file.
data SourceLoc = SourceLoc
  { sourcePath :: !Text
  , sourceLine :: !Int
  }
  deriving stock (Show, Eq)

compactKeys, fullKeys :: Set Text
compactKeys = Set.fromList
  [ "name", "kind", "package", "version", "module"
  , "signature", "haddock_raw", "source"
  ]
fullKeys = compactKeys

-- | Execute the @symbol@ command.
--
-- The argument must have the form @PKG/MOD/SYM@ (a 'SymbolPath' with all
-- three segments).  We look up the package version in the build plan, find
-- the source directory via 'BuildEnv', read the module file, and extract
-- the symbol's signature, Haddock block, and definition line.
runSymbol
  :: BuildEnv IO
  -> BuildPlan
  -> Text
  -> IO (Either HyphaError (Outcome Value))
runSymbol env plan rawArg =
  case parseSymbolPath rawArg of
    Left _ -> pure $ Left $ UserError
      ("expected PKG/MOD/SYM (got: " <> rawArg <> ")")
    Right sp -> case (spModule sp, spSymbol sp) of
      (Nothing, _) -> pure $ Left $ UserError
        ("expected PKG/MOD/SYM — module segment missing (got: " <> rawArg <> ")")
      (_, Nothing) -> pure $ Left $ UserError
        ("expected PKG/MOD/SYM — symbol segment missing (got: " <> rawArg <> ")")
      (Just modPath, Just symName) -> do
        let pkgName = spPackage sp
            sym     = unSymbolName symName
            modTxt  = unModulePath modPath
        case lookupPackage pkgName plan of
          Nothing -> pure $ Left $ NotFound
            ("package '" <> unPackageName pkgName <> "' not in build plan (use --any to widen)")
          Just ver -> do
            let pid = PackageId pkgName ver
            mDir <- locatePackageSource env pid
            case mDir of
              Nothing -> pure $ Left $ EnvError
                ("source directory not found for " <> unPackageName pkgName <> "-" <> unVersion ver)
              Just d  -> do
                let f = d </> modulePathToFile modTxt
                ok <- doesFileExist f
                if not ok
                  then pure $ Left $ NotFound
                    ("module file not found: " <> Text.pack f)
                  else do
                    src <- TIO.readFile f
                    let info = extractSymbolInfo src sym
                    pure $ Right $ mkOutcome pkgName ver modTxt sym f info

mkOutcome
  :: PackageName
  -> Version
  -> Text
  -> Text
  -> FilePath
  -> SymbolInfo
  -> Outcome Value
mkOutcome pkgName ver modTxt sym f info =
  let pkg = unPackageName pkgName
      result = SymbolResult
        { srName      = sym
        , srKind      = "function"
        , srPackage   = pkg
        , srVersion   = unVersion ver
        , srModule    = modTxt
        , srSignature = siSignature info
        , srHaddock   = unDocText <$> siHaddock info
        , srSource    = case siLine info of
            Nothing -> Nothing
            Just ln -> Just (SourceLoc (Text.pack f) ln)
        }
      body   = symbolResultToJSON result
      actions = Map.fromList
        [ ("view_source", "hypha source " <> pkg <> "/" <> modTxt <> "/" <> sym)
        , ("module_index", "hypha module " <> pkg <> "/" <> modTxt)
        , ("package_info", "hypha package " <> pkg)
        ]
      related =
        [ Related "module_index" ("hypha module " <> pkg <> "/" <> modTxt)
        , Related "package" ("hypha package " <> pkg)
        ]
  in OutcomeSuccess body False [] actions related

symbolResultToJSON :: SymbolResult -> Value
symbolResultToJSON r = object $ concat
  [ [ "name"      .= srName r
    , "kind"      .= srKind r
    , "package"   .= srPackage r
    , "version"   .= srVersion r
    , "module"    .= srModule r
    ]
  , [ "signature" .= s | Just s <- [srSignature r] ]
  , [ "haddock_raw" .= h | Just h <- [srHaddock r] ]
  , [ "source"    .= object [ "path" .= sourcePath s, "line" .= sourceLine s ]
    | Just s <- [srSource r]
    ]
  ]
