-- | Two hops from the definition: re-exports what 'Fixture.Wrapper'
-- re-exports from 'Fixture.Internal'.  Mirrors @Data.Map@, which
-- re-exports @Data.Map.Lazy@, which re-exports @Data.Map.Internal@.
module Fixture.Facade
  ( insertBag
  ) where

import Fixture.Wrapper (insertBag)
