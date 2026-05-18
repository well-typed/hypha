{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Command.Symbol
  ( SymbolResult (..)
  , compactKeys
  , fullKeys
  , runSymbol
  , runSymbolWith
  ) where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
import Data.Bifunctor (first)
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
import Hypha.Output.Outcome (Outcome (..), Related (..), tagOutsidePlan)
import Hypha.Package.Resolver (PackageResolver (..), ResolvedPackage (..))
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
runSymbol env plan rawArg = runExceptT $ do
  sp        <- liftParseError rawArg (parseSymbolPath rawArg)
  (modPath, symName) <- requireModuleAndSymbol sp rawArg
  let pkgName = spPackage sp
      sym     = unSymbolName symName
      modTxt  = unModulePath modPath
  ver       <- liftMaybe (NotFound ("package '" <> unPackageName pkgName
                              <> "' not in build plan"))
                (lookupPackage pkgName plan)
  let pid = PackageId pkgName ver
  d         <- liftMaybe (EnvError ("source directory not found for "
                              <> unPackageName pkgName <> "-" <> unVersion ver))
                =<< liftIO (locatePackageSource env pid)
  let f = d </> modulePathToFile modTxt
  ok        <- liftIO (doesFileExist f)
  unless ok (throwE (NotFound ("module file not found: " <> Text.pack f)))
  src       <- liftIO (TIO.readFile f)
  let info = extractSymbolInfo src sym
  pure (mkOutcome pkgName ver modTxt sym f info)

-- | Execute the @symbol@ command using the package resolver instead of a raw build plan.
--
-- Falls through plan -> store -> Hackage to resolve the package, then proceeds
-- with source extraction just like 'runSymbol'.
runSymbolWith
  :: BuildEnv IO
  -> PackageResolver IO
  -> Text
  -> IO (Either HyphaError (Outcome Value))
runSymbolWith env resolver rawArg = runExceptT $ do
  sp        <- liftParseError rawArg (parseSymbolPath rawArg)
  (modPath, symName) <- requireModuleAndSymbol sp rawArg
  let pkgName = spPackage sp
      sym     = unSymbolName symName
      modTxt  = unModulePath modPath
  rp        <- ExceptT $ resolvePkg resolver pkgName
  let pid = rpPkgId rp
      ver = pkgVersion pid
  d         <- liftMaybe (EnvError ("source directory not found for "
                              <> unPackageName pkgName <> "-" <> unVersion ver))
                =<< liftIO (locatePackageSource env pid)
  let f = d </> modulePathToFile modTxt
  ok        <- liftIO (doesFileExist f)
  unless ok (throwE (NotFound ("module file not found: " <> Text.pack f)))
  src       <- liftIO (TIO.readFile f)
  let info = extractSymbolInfo src sym
      outcome = mkOutcome pkgName ver modTxt sym f info
  pure (tagOutsidePlan outcome (rpIsOutsidePlan rp))

-- | Convert a 'SymbolPath' parse failure into a 'UserError'.
liftParseError :: Text -> Either e SymbolPath -> ExceptT HyphaError IO SymbolPath
liftParseError rawArg = ExceptT . pure . first (const (UserError
  ("expected PKG/MOD/SYM (got: " <> rawArg <> ")")))

-- | Require both module and symbol segments from a 'SymbolPath'.
requireModuleAndSymbol :: SymbolPath -> Text -> ExceptT HyphaError IO (ModulePath, SymbolName)
requireModuleAndSymbol sp rawArg =
  case (spModule sp, spSymbol sp) of
    (Just m, Just s) -> pure (m, s)
    (Nothing, _)     -> throwE $ UserError
      ("expected PKG/MOD/SYM — module segment missing (got: " <> rawArg <> ")")
    (_, Nothing)     -> throwE $ UserError
      ("expected PKG/MOD/SYM — symbol segment missing (got: " <> rawArg <> ")")

-- | Lift a 'Maybe' into 'ExceptT' with the given error on 'Nothing'.
liftMaybe :: Monad m => HyphaError -> Maybe a -> ExceptT HyphaError m a
liftMaybe err = maybe (throwE err) pure

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
        , srSource    = mkSourceLoc f <$> siLine info
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

-- | Build a 'SourceLoc' from a file path and a line number.
mkSourceLoc :: FilePath -> Int -> SourceLoc
mkSourceLoc f ln = SourceLoc (Text.pack f) ln

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
