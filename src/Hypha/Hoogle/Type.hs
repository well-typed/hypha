{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hypha.Hoogle.Type
  ( HoogleQuery (..)
  , HoogleHit (..)
  , Hoogle (..)
  ) where

import Control.DeepSeq (NFData)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | A Hoogle search query string.
newtype HoogleQuery = HoogleQuery { unHoogleQuery :: Text }
  deriving stock (Show, Eq)
  deriving newtype (NFData)

-- | A single hit from a Hoogle search.
data HoogleHit = HoogleHit
  { hhPackage :: !Text
  , hhModule  :: !Text
  , hhName    :: !Text
  , hhSig     :: !Text
  , hhDocs    :: !Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

-- | Record-of-functions interface for Hoogle operations.
-- Parameterized over @m@ following the project convention.
data Hoogle m = Hoogle
  { searchHoogle  :: !(HoogleQuery -> m [HoogleHit])
  , ensureFreshDb :: !(m ())
  }
