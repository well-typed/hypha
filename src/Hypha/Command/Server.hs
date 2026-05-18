{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | The @hypha server@ subcommand.
--
-- Boots a local doc browser (Warp + WAI) bound to loopback only.  Optional
-- prebuild stage walks the build plan and renders Haddocks concurrently so
-- the first request lands on a warm cache.
module Hypha.Command.Server
  ( -- * Types
    ServerOpts (..)
  , BindAddr (..)
  , BindError (..)
    -- * Bind parsing
  , parseBind
    -- * Entry points
  , runServer
  , buildServerConfig
  ) where

import Control.Concurrent.QSem (newQSem, signalQSem, waitQSem)
import Control.Concurrent.Async (mapConcurrently_)
import Control.Exception (SomeException, bracket_, try)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as TIO
import Network.Wai.Handler.Warp
  ( defaultSettings, runSettings, setHost, setPort )
import qualified Data.String as String
import System.IO (hPutStrLn, stderr)

import Hypha.BuildEnv.Type (BuildEnv (..))
import Hypha.Hackage.Api (HackageClient (..))
import Hypha.Haddock.Generate (ensureHaddockFor, haddockDirFor)
import Hypha.Hoogle.Type (Hoogle (..), HoogleQuery (..), HoogleHit (..))
import Hypha.Package.Resolver
  ( PackageResolver (..), ResolvedPackage (..) )
import qualified Hypha.Server.App as App
import qualified Hypha.Server.Haddock.Rewrite as Rewrite
import qualified Hypha.Server.Slots as Slots
import qualified Hypha.Source.Extract as Extract
import qualified Hypha.Source.Locate as Locate
import Hypha.Types.BuildPlan (BuildPlan (..), PlannedUnit (..))
import Hypha.Types.Doc (DocText (..))
import Hypha.Types.PackageId
  ( PackageId (..), PackageName (..), Version (..) )
import qualified System.Directory as Dir
import qualified System.FilePath as FP

-- | Bind address.  Loopback only — explicit guard in 'parseBind'.
data BindAddr = BindAddr
  { baHost :: !String
  , baPort :: !Int
  }
  deriving stock (Show, Eq)

-- | Parse failure or refusal.
data BindError
  = BindMalformed   !Text  -- ^ Could not parse @HOST:PORT@.
  | BindNonLoopback !Text  -- ^ Caller asked for a non-loopback bind.
  deriving stock (Show, Eq)

-- | All @hypha server@ options.
data ServerOpts = ServerOpts
  { soBind         :: !BindAddr
    -- ^ Resolved bind address.
  , soPrebuild     :: !Bool
    -- ^ Walk plan and pre-render Haddocks.
  , soPrebuildJobs :: !Int
    -- ^ Max concurrent prebuild workers.
  }
  deriving stock (Show, Eq)

-- | Parse a bind string of the form @HOST:PORT@.  Only loopback hosts are
-- accepted — anything else returns 'BindNonLoopback'.
parseBind :: Text -> Either BindError BindAddr
parseBind raw =
  case Text.splitOn ":" raw of
    [h, p] | Just port <- readPortMaybe (Text.unpack p) ->
      if isLoopback h
        then Right (BindAddr (Text.unpack h) port)
        else Left  (BindNonLoopback raw)
    _ -> Left (BindMalformed raw)
  where
    isLoopback h = h == "localhost" || h == "127.0.0.1" || h == "::1"
    readPortMaybe s = case reads s of
      [(n, "")] | n >= 1 && n <= 65535 -> Just n
      _                                -> Nothing

-- | Boot the server.  Returns 'Left' on bind refusal; otherwise blocks
-- inside Warp's event loop.
runServer
  :: BuildPlan
  -> BuildEnv IO
  -> HackageClient IO
  -> PackageResolver IO
  -> Hoogle IO
  -> ServerOpts
  -> IO (Either BindError ())
runServer plan env hclient resolver hoogle opts = do
  cfg <- buildServerConfig plan env hclient resolver hoogle
  hPutStrLn stderr
    ( "hypha server listening on http://" <> baHost (soBind opts)
   <> ":" <> show (baPort (soBind opts))
    )
  case soPrebuild opts of
    False -> pure ()
    True  -> prebuildAll env (soPrebuildJobs opts) (planPackageIds plan)
  let settings = setHost (String.fromString (baHost (soBind opts)))
               $ setPort (baPort (soBind opts))
                 defaultSettings
  runSettings settings (App.appWith cfg)
  pure (Right ())

-- | Concurrently warm the Haddock cache for every package in the plan.
prebuildAll :: BuildEnv IO -> Int -> [PackageId] -> IO ()
prebuildAll env jobs pids = do
  sem <- newQSem (max 1 jobs)
  mapConcurrently_ (withSem sem . ensureOne) pids
  where
    ensureOne pid = do
      r <- try (ensureHaddockFor env pid) :: IO (Either SomeException (Maybe FilePath))
      case r of
        Right (Just _) -> pure ()
        _              -> pure ()
    withSem sem action = bracket_ (waitQSem sem) (signalQSem sem) action

-- | Extract every distinct 'PackageId' from a plan.
planPackageIds :: BuildPlan -> [PackageId]
planPackageIds = map puId . Map.elems . bpUnits

-- | Assemble the 'ServerConfig' callbacks that connect the WAI app to the
-- resolver, build env, and Hoogle.
buildServerConfig
  :: BuildPlan
  -> BuildEnv IO
  -> HackageClient IO
  -> PackageResolver IO
  -> Hoogle IO
  -> IO App.ServerConfig
buildServerConfig plan env _hclient resolver hoogle = do
  let pids      = planPackageIds plan
      packages  = map (unPackageName . pkgName) pids
  slots <- Slots.initialiseSlots pids
  pure App.ServerConfig
    { App.scProjectName  = projectName plan
    , App.scPackages     = packages
    , App.scSlots        = slots
    , App.scHumanSearch  = \q -> do
        hits <- searchHoogle hoogle (HoogleQuery q)
        pure
          [ (hhPackage h, hhModule h, hhName h, hhSig h)
          | h <- hits
          ]
    , App.scSymbolLookup = \pkgT modT symT -> do
        ePid <- resolvePkg resolver (PackageName pkgT)
        case ePid of
          Left _ -> pure Nothing
          Right rp -> do
            let pid = rpPkgId rp
            mDir <- locatePackageSource env pid
            case mDir of
              Nothing  -> pure Nothing
              Just dir -> do
                let file = dir FP.</> Locate.modulePathToFile modT
                exists <- Dir.doesFileExist file
                if not exists
                  then pure Nothing
                  else do
                    src <- TIO.readFile file
                    let info = Extract.extractSymbolInfo src symT
                        sig  = maybe "" id (Extract.siSignature info)
                        hd   = maybe "" unDocText (Extract.siHaddock info)
                        ln   = maybe 1 id (Extract.siLine info)
                    pure (Just (sig, hd, Text.pack file, ln))
    , App.scHaddockHtml  = \pkgVer segments -> do
        let pidM = parsePkgVer pkgVer
        case pidM of
          Nothing  -> pure Nothing
          Just pid -> do
            dir <- haddockDirFor pid
            let path = foldl (FP.</>) dir segments
            exists <- Dir.doesFileExist path
            if not exists
              then pure Nothing
              else do
                bs <- LBS.readFile path
                let txt = Text.decodeUtf8 (LBS.toStrict bs)
                pure (Just (LBS.fromStrict (Text.encodeUtf8 (Rewrite.rewriteHaddockHtml txt))))
    , App.scSourceText   = \pkgT modT -> do
        ePid <- resolvePkg resolver (PackageName pkgT)
        case ePid of
          Left _ -> pure Nothing
          Right rp -> do
            mDir <- locatePackageSource env (rpPkgId rp)
            case mDir of
              Nothing  -> pure Nothing
              Just dir -> do
                let file = dir FP.</> Locate.modulePathToFile modT
                exists <- Dir.doesFileExist file
                if not exists
                  then pure Nothing
                  else Just <$> TIO.readFile file
    }

-- | Project name (best-effort).  Uses the first local package, or a
-- placeholder when none are present.
projectName :: BuildPlan -> Text
projectName plan =
  case filter puIsLocal (Map.elems (bpUnits plan)) of
    (pu : _) -> unPackageName (pkgName (puId pu))
    []       -> "hypha"

-- | Parse @"<pkg>-<ver>"@.  The version is the suffix after the last @-@.
parsePkgVer :: Text -> Maybe PackageId
parsePkgVer raw =
  case Text.breakOnEnd "-" raw of
    (pre, ver) | not (Text.null pre) && not (Text.null ver) ->
      let name = Text.dropEnd 1 pre
      in Just (PackageId (PackageName name) (Version ver))
    _ -> Nothing

