{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Hypha.Hoogle.Query
  ( mkProjectHoogle
  , mkGlobalHoogle
  , mkHoogleForFlags
  ) where

import Control.Exception (SomeException, try)
import qualified Data.Text as Text
import qualified Hoogle

import Hypha.Cli.Parser (GlobalFlags (..))
import Hypha.Hoogle.Database (HoogleConfig (..), withProjectDb, withGlobalDb)
import Hypha.Hoogle.Type     (Hoogle (..), HoogleHit (..), HoogleQuery (..))
import Hypha.Project.Discovery (discoverProjectRoot)
import Hypha.Project.Plan (loadBuildPlan)

-- | Create a Hoogle interface backed by the per-project database.  Any
-- exception (missing DB, failed generate, concurrent access) collapses to
-- an empty result list so callers can fall back to other indices.
mkProjectHoogle :: HoogleConfig -> IO (Hoogle IO)
mkProjectHoogle cfg = pure Hoogle
  { searchHoogle  = \q -> safeSearch
      $ withProjectDb cfg
      $ \db -> pure (map toHit (Hoogle.searchDatabase db (Text.unpack (unHoogleQuery q))))
  , ensureFreshDb = safeUnit (withProjectDb cfg (\_ -> pure ()))
  }

-- | Create a Hoogle interface backed by the global database.
mkGlobalHoogle :: IO (Hoogle IO)
mkGlobalHoogle = pure Hoogle
  { searchHoogle  = \q -> safeSearch
      $ withGlobalDb
      $ \db -> pure (map toHit (Hoogle.searchDatabase db (Text.unpack (unHoogleQuery q))))
  , ensureFreshDb = safeUnit (withGlobalDb (\_ -> pure ()))
  }

safeSearch :: IO [HoogleHit] -> IO [HoogleHit]
safeSearch act = do
  r <- try act :: IO (Either SomeException [HoogleHit])
  case r of
    Left _  -> pure []
    Right x -> pure x

safeUnit :: IO () -> IO ()
safeUnit act = do
  r <- try act :: IO (Either SomeException ())
  case r of
    Left _  -> pure ()
    Right _ -> pure ()

-- | Convert a Hoogle Target to our HoogleHit type.
toHit :: Hoogle.Target -> HoogleHit
toHit t = HoogleHit
  { hhPackage = maybe "" (Text.pack . fst) (Hoogle.targetPackage t)
  , hhModule  = maybe "" (Text.pack . fst) (Hoogle.targetModule t)
  , hhName    = Text.pack (Hoogle.targetItem t)
  , hhSig     = Text.pack (Hoogle.targetType t)
  , hhDocs    = Text.pack (Hoogle.targetDocs t)
  }

-- | Build a 'Hoogle IO' record from global flags.
--
-- If @--global@ is set, use the global Stackage DB.
-- Otherwise try to discover the project root, load its plan, and build a
-- per-project DB.  Fall back to the global DB on any failure.
mkHoogleForFlags :: GlobalFlags -> IO (Hoogle IO)
mkHoogleForFlags flags =
  if gfGlobal flags
    then mkGlobalHoogle
    else do
      eRoot <- discoverProjectRoot (gfProjectDir flags)
      case eRoot of
        Left _  -> mkGlobalHoogle
        Right root -> do
          ePlan <- loadBuildPlan root
          case ePlan of
            Left _  -> mkGlobalHoogle
            Right _ -> mkProjectHoogle (HoogleConfig root [] "alpha-stub")
