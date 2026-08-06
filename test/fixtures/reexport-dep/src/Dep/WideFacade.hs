-- | A facade whose candidate ring is dominated by open imports.
--
-- This is @base@'s @Control.Concurrent@ shape, which issue #20 named as
-- the second fixture worth having: an unrestricted import is a candidate
-- for every name, so the ring is wide and the module that really declares
-- the symbol is not the best-ranked entry in it.  Only @Dep.Internal@ is
-- imported explicitly, and it is imported last.
module Dep.WideFacade
  ( depThing
  ) where

import Dep.Filler1
import Dep.Filler2
import Dep.Filler3
import Dep.Filler4
import Dep.Homonym
import Dep.Internal (depThing)
