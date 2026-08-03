-- | The @base:Control.Concurrent@ shape: an open import that supplies
-- nothing stands in front of the one that really does.
--
-- @Fixture.Bystander@ wins the ranking -- it shares a segment with this
-- module and @Dep.Internal@ shares none -- but it has no @depThing@, so
-- resolution has to move on to the next candidate rather than give up on
-- the first.  Naming only the first is how @base@ lost
-- @Control.Concurrent.isCurrentThreadBound@ to @import Prelude@.
module Fixture.Shadowed
  ( depThing
  ) where

import Fixture.Bystander
import Dep.Internal
