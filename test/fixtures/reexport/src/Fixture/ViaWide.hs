-- | Asks through a facade whose own ring is wide.
--
-- @Dep.WideFacade@ declares nothing and offers six candidates, five of
-- them open imports, one of which declares a private @depThing@.  Getting
-- to @Dep.Internal@ requires both halves of the ranking to survive the
-- descent: explicit imports ahead of open ones, and \"declares it\" not
-- being mistaken for \"exports it\".
module Fixture.ViaWide
  ( depThing
  ) where

import Dep.WideFacade
