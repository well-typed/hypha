{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Command.Symbol
  ( SymbolResult (..)
  , compactKeys
  , fullKeys
  , runSymbolWith
  ) where

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

import Hypha.BuildEnv.Type (BuildEnv)
import Hypha.Command.Source qualified as Source
import Hypha.Error (HyphaError (..), UserErrorReason (..))
import Hypha.Output.Outcome (Outcome (..), tagOutsidePlan)
import Hypha.Package.Resolver
  ( PackageResolver (..), ResolvedPackage (..), resolveRef )
import Hypha.Prelude (warnOnLeft)
import Hypha.Source.Extract
  (SymbolInfo (..), extractSymbolInfo, noSymbolInfo)
import Hypha.Source.Extract qualified as Extract
import Hypha.Source.Locate qualified as Locate
import Hypha.Source.Parser (parseErrorMessage, renderDeclKind)
import Hypha.Source.Reach (OutsideReach)
import Hypha.Types.ComponentName (ComponentKey (..))
import Hypha.Types.PackageId
  ( PackageId (..), PackageName (..), PackageRef (..), Version (..) )
import Hypha.Types.SymbolPath
  ( SymbolPath (..), ModulePath (..), SymbolName (..), parseSymbolPath )
import Hypha.Types.Doc (DocText (..))
import Hypha.Cli.Types

-- | Result of the @symbol@ command.
data SymbolResult = SymbolResult
  { srName      :: !Text
  , srKind      :: !(Maybe Text)
    -- ^ What the parser classified the declaration as.
    --
    -- Absent rather than defaulted: this field used to be the constant
    -- @\"function\"@, which made the card assert the one thing it had not
    -- learned — including for a symbol whose declaration was never found
    -- at all.
  , srPackage   :: !Text
  , srVersion   :: !Text
  , srModule    :: !Text
  , srSignature :: !(Maybe Text)
  , srHaddock   :: !(Maybe Text)
  , srSource    :: !(Maybe SourceLoc)
  , srDefinedIn :: !(Maybe DefinedIn)
    -- ^ Where the declaration turned out to be, when the module the
    -- caller named only re-exports it.
  }
  deriving stock (Show, Eq)

-- | Source location within a file.
data SourceLoc = SourceLoc
  { sourcePath :: !Text
  , sourceLine :: !Int
  }
  deriving stock (Show, Eq)

-- | The module and component a declaration turned out to live in.
data DefinedIn = DefinedIn
  { diModule    :: !ModulePath
  , diComponent :: !ComponentKey
  }
  deriving stock (Show, Eq)

compactKeys, fullKeys :: Set Text
compactKeys = Set.fromList
  [ "name", "kind", "package", "version", "module"
  , "signature", "haddock_raw", "defined_in"
  ]
fullKeys = Set.fromList
  [ "name", "kind", "package", "version", "module"
  , "signature", "haddock_raw", "source", "defined_in"
  ]

-- | Execute the @symbol@ command.
--
-- The argument must have the form @PKG/MOD/SYM@ (a 'SymbolPath' with all
-- three segments).  Falls through plan -> store -> Hackage to resolve the
-- package, then resolves the symbol through its component's exports — the
-- same path @hypha source@ takes, and for the same reason.
--
-- Reading the named module's own file, which is what this used to do,
-- answers nothing at all about a facade: @hypha symbol
-- base\/Data.List\/sortOn@ found @Data.List@, found no @sortOn@ declared in
-- it, and returned a card with no signature, no Haddock and no source —
-- while @hypha source@ on the same argument landed on the definition.
-- Following the re-export is what closes that gap, and a symbol that
-- genuinely is not there is now an error rather than an empty card.
runSymbolWith
  :: Maybe FilePath
    -- ^ The plan's synthesised @cabal_macros.h@.
  -> BuildEnv IO
  -> PackageResolver IO
  -> (PackageId -> IO (OutsideReach IO))
    -- ^ The dependency closure of the package asked about, for a re-export
    -- that leaves it.  A producer rather than a reach, because which
    -- package that is only becomes known once the argument is parsed.
  -> Text
  -> IO (Either HyphaError (Outcome Value))
runSymbolWith mMacroHeader _env resolver mkReach rawArg = runExceptT $ do
  sp                 <- liftParseError rawArg (parseSymbolPath rawArg)
  (modPath, symName) <- requireModuleAndSymbol sp rawArg
  let ref = PackageRef (spPackage sp) (spVersion sp)
  rp      <- ExceptT (resolveRef resolver ref)
  let pid = rpPkgId rp
  d       <- ExceptT (resolveSrc resolver pid)
  reach   <- liftIO (mkReach pid)
  located <- liftIO
    (Source.locateSymbolSite mMacroHeader reach pid d modPath symName)
  card    <- case located of
    Right site -> liftIO (cardFor symName site)
    Left err   -> throwE
      =<< liftIO (Source.symbolNotFound reach pid modPath symName err)
  pure (tagOutsidePlan (mkOutcome pid modPath symName card)
          (rpIsOutsidePlan rp))

-- | What a card says, and where it was read from.
data CardSource = CardSource
  { csInfo      :: !SymbolInfo
  , csPath      :: !FilePath
  , csDefinedIn :: !(Maybe DefinedIn)
  }

-- | Build the card from wherever the symbol turned out to be.
--
-- The resolved arm reads the declaration off the parse that located it —
-- under the settings of the stanza that module belongs to, and without
-- touching the disk a second time.  The swept arm has only a line number,
-- so it parses the file it landed in, which is all a package with no
-- readable cabal allows.
cardFor :: SymbolName -> Source.SymbolSite -> IO CardSource
cardFor sym site = case site of
  Source.ResolvedSite ld -> pure CardSource
    { csInfo      = Extract.symbolInfoFromDecl
                      (Extract.numberedLines (Locate.ldContent ld))
                      (Locate.ldDecl ld)
    , csPath      = Locate.slPath (Locate.ldLocation ld)
    , csDefinedIn = Just (DefinedIn (Locate.ldModule ld) (Locate.ldComponent ld))
    }
  Source.SweptSite loc -> do
    let f = Locate.slPath loc
    src  <- TIO.readFile f
    info <- symbolInfoOf f src (unSymbolName sym)
    pure (CardSource info f Nothing)

-- | Extract the symbol's information, announcing a parse failure rather
-- than presenting an empty card as if the module simply had nothing to
-- say about the symbol.
symbolInfoOf :: FilePath -> Text -> Text -> IO SymbolInfo
symbolInfoOf f src sym =
  warnOnLeft render noSymbolInfo (pure (extractSymbolInfo src sym))
  where
    render e = Text.pack f <> " could not be parsed: " <> parseErrorMessage e

-- | Convert a 'SymbolPath' parse failure into a 'UserError'.
liftParseError :: Text -> Either e SymbolPath -> ExceptT HyphaError IO SymbolPath
liftParseError rawArg = ExceptT . pure . first
  (const (UserError (UserExpectedSymbolPath rawArg)))

-- | Require both module and symbol segments from a 'SymbolPath'.
requireModuleAndSymbol :: SymbolPath -> Text -> ExceptT HyphaError IO (ModulePath, SymbolName)
requireModuleAndSymbol sp rawArg =
  case (spModule sp, spSymbol sp) of
    (Just m, Just s) -> pure (m, s)
    (Nothing, _)     -> throwE (UserError (UserSymbolPathMissingModule rawArg))
    (_, Nothing)     -> throwE (UserError (UserSymbolPathMissingSymbol rawArg))

mkOutcome
  :: PackageId
  -> ModulePath
  -> SymbolName
  -> CardSource
  -> Outcome Value
mkOutcome pid modPath symName card =
  let pkg    = unPackageName (pkgName pid)
      modTxt = unModulePath modPath
      sym    = unSymbolName symName
      info   = csInfo card
      result = SymbolResult
        { srName      = sym
        , srKind      = renderDeclKind <$> siKind info
        , srPackage   = pkg
        , srVersion   = unVersion (pkgVersion pid)
        , srModule    = modTxt
        , srSignature = siSignature info
        , srHaddock   = unDocText <$> siHaddock info
        , srSource    = mkSourceLoc (csPath card) <$> siLine info
        -- Only when it says something the caller does not already know:
        -- a definition in the module asked for needs no annotation.
        , srDefinedIn = case csDefinedIn card of
            Just d | diModule d /= modPath -> Just d
            _                              -> Nothing
        }
      body   = symbolResultToJSON result
      actions = Map.fromList
        [ ("view_source",  "hypha source "  <> pkg <> "/" <> modTxt <> "/" <> sym)
        , ("module_index", "hypha module "  <> pkg <> "/" <> modTxt)
        , ("package_info", "hypha package " <> pkg)
        ]
  in Outcome body SymbolCmd False [] actions

-- | Build a 'SourceLoc' from a file path and a line number.
mkSourceLoc :: FilePath -> Int -> SourceLoc
mkSourceLoc f ln = SourceLoc (Text.pack f) ln

symbolResultToJSON :: SymbolResult -> Value
symbolResultToJSON r = object $ concat
  [ [ "name"      .= srName r ]
  , [ "kind"      .= k | Just k <- [srKind r] ]
  , [ "package"   .= srPackage r
    , "version"   .= srVersion r
    , "module"    .= srModule r
    ]
  , [ "signature" .= s | Just s <- [srSignature r] ]
  , [ "haddock_raw" .= h | Just h <- [srHaddock r] ]
  , [ "source"    .= object [ "path" .= sourcePath s, "line" .= sourceLine s ]
    | Just s <- [srSource r]
    ]
  , [ "defined_in" .= object
        [ "module"    .= unModulePath (diModule d)
        , "component" .= unComponentKey (diComponent d)
        ]
    | Just d <- [srDefinedIn r]
    ]
  ]
