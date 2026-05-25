{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
-- | Hoogle / lookup tier classification.
--
-- Lives in its own leaf module so both 'Hypha.Error' and
-- 'Hypha.Command.Lookup' can refer to 'Tier' values without forming an
-- import cycle.  Rendering to 'Text' lives here and is the /only/
-- conversion path — error constructors carry @[Tier]@, not pre-rendered
-- comma-joined strings.
module Hypha.Hoogle.Tier
  ( Tier (..)
  , tierLabel
  , renderTierList
  ) where

import Data.Text (Text)
import qualified Data.Text as Text

-- | Which tier produced (or failed to produce) a hit.
data Tier = TierCache | TierLocalHoogle | TierRemoteHoogle
  deriving stock (Show, Eq, Ord)

-- | Stable wire label for a single tier.
tierLabel :: Tier -> Text
tierLabel = \case
  TierCache         -> "cache"
  TierLocalHoogle   -> "local-hoogle"
  TierRemoteHoogle  -> "remote-hoogle"

-- | Comma-joined tier list for the @tiers_consulted@ envelope action.
renderTierList :: [Tier] -> Text
renderTierList = Text.intercalate "," . map tierLabel
