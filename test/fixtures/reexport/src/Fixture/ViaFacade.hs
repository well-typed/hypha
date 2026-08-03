-- | Two candidate suppliers, both reachable, only one of which declares
-- the symbol.
--
-- @Dep.Facade@ ranks ahead of @Dep.Internal@ and passes @depThing@ along
-- without declaring it.  A locator that stops at the first candidate whose
-- source it happens to have reports the symbol absent -- the same
-- "commit to the first plausible import" mistake the resolver was cured
-- of, one layer down.
module Fixture.ViaFacade
  ( depThing
  ) where

import Dep.Facade
import Dep.Internal
