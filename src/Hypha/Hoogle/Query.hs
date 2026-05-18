{-# LANGUAGE OverloadedStrings #-}
module Hypha.Hoogle.Query
  ( mkProjectHoogle
  , mkGlobalHoogle
  ) where

import qualified Data.Text as Text
import qualified Hoogle

import Hypha.Hoogle.Database (HoogleConfig (..), withProjectDb, withGlobalDb)
import Hypha.Hoogle.Type     (Hoogle (..), HoogleHit (..), HoogleQuery (..))

-- | Create a Hoogle interface backed by the per-project database.
mkProjectHoogle :: HoogleConfig -> IO (Hoogle IO)
mkProjectHoogle cfg = pure Hoogle
  { searchHoogle  = \q -> withProjectDb cfg $ \db ->
      pure (map toHit (Hoogle.searchDatabase db (Text.unpack (unHoogleQuery q))))
  , ensureFreshDb = withProjectDb cfg (\_ -> pure ())
  }

-- | Create a Hoogle interface backed by the global database.
mkGlobalHoogle :: IO (Hoogle IO)
mkGlobalHoogle = pure Hoogle
  { searchHoogle  = \q -> withGlobalDb $ \db ->
      pure (map toHit (Hoogle.searchDatabase db (Text.unpack (unHoogleQuery q))))
  , ensureFreshDb = withGlobalDb (\_ -> pure ())
  }

-- | Convert a Hoogle Target to our HoogleHit type.
toHit :: Hoogle.Target -> HoogleHit
toHit t = HoogleHit
  { hhPackage = maybe "" (Text.pack . fst) (Hoogle.targetPackage t)
  , hhModule  = maybe "" (Text.pack . fst) (Hoogle.targetModule t)
  , hhName    = Text.pack (Hoogle.targetItem t)
  , hhSig     = Text.pack (Hoogle.targetType t)
  , hhDocs    = Text.pack (Hoogle.targetDocs t)
  }
