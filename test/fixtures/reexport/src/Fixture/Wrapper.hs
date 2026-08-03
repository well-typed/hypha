-- | Public face of the fixture package: re-exports the internal
-- definitions the way @Data.Map.Strict@ re-exports
-- @Data.Map.Strict.Internal@.
module Fixture.Wrapper
  ( Bag (..)
  , insertBag
  , module Fixture.Other
  ) where

import Fixture.Internal (Bag (..), insertBag)
import Fixture.Other
