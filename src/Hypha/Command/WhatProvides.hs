{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Command.WhatProvides
  ( WhatProvidesResult (..)
  , Provider (..)
  , compactKeys
  , fullKeys
  , runWhatProvides
  , runWhatProvidesWith
  , providerToJSON
  , whatProvidesResultToJSON
  , fromHit
  ) where

import Data.Aeson (Value, object, (.=))
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Text (Text)

import Hypha.Hoogle.Type (Hoogle (..), HoogleHit (..), HoogleQuery (..))
import Hypha.Output.Outcome (Outcome (..), Related (..))

-- | Result of the @whatprovides@ command.
data WhatProvidesResult = WhatProvidesResult
  { wprSymbol    :: !Text
  , wprProviders :: ![Provider]
  }
  deriving stock (Show, Eq)

-- | A single package/module that provides the symbol.
data Provider = Provider
  { pPackage :: !Text
  , pModule  :: !Text
  , pFetch   :: !Text
  }
  deriving stock (Show, Eq)

compactKeys, fullKeys :: Set Text
compactKeys = Set.fromList ["symbol", "providers"]
fullKeys    = compactKeys

-- | Run @whatprovides@ against the supplied 'Hoogle' record.
runWhatProvidesWith :: Monad m => Hoogle m -> Text -> Bool -> m (Outcome Value)
runWhatProvidesWith hoogle sym wasGlobal = do
  hits <- searchHoogle hoogle (HoogleQuery ("is:exact " <> sym))
  let providers = map (fromHit sym) hits
      body      = WhatProvidesResult sym providers
      actions = if null providers && not wasGlobal
                  then Map.singleton "retry_with_global"
                         ("hypha whatprovides " <> sym <> " --global")
                  else mempty
      related   =
        [ Related (pPackage p <> "/" <> pModule p) (pFetch p)
        | p <- take 5 providers
        ]
  pure (OutcomeSuccess (whatProvidesResultToJSON body) False [] actions related)

-- | IO convenience wrapper: builds the query and delegates to
-- 'runWhatProvidesWith'.  Kept for backward compatibility with the CLI
-- dispatcher.
runWhatProvides :: Hoogle IO -> Text -> Bool -> IO (Outcome Value)
runWhatProvides = runWhatProvidesWith

fromHit :: Text -> HoogleHit -> Provider
fromHit sym h = Provider
  { pPackage = hhPackage h
  , pModule  = hhModule h
  , pFetch   = "hypha symbol " <> hhPackage h <> "/" <> hhModule h <> "/" <> sym
  }

whatProvidesResultToJSON :: WhatProvidesResult -> Value
whatProvidesResultToJSON (WhatProvidesResult sym ps) = object
  [ "symbol"     .= sym
  , "providers"  .= map providerToJSON ps
  ]

providerToJSON :: Provider -> Value
providerToJSON p = object
  [ "package" .= pPackage p
  , "module"  .= pModule p
  , "fetch"   .= pFetch p
  ]
