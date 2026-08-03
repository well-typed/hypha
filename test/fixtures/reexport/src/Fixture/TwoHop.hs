-- | Two hops from the declaration, across a package boundary: this module
-- reaches @Dep.Facade@, which reaches @Dep.Internal@.  The @base@ shape for
-- @Data.List.mapAccumL@ — declared in @GHC.Internal.Data.Traversable@ and
-- reached through @GHC.Internal.Data.List@, which only passes it along.
module Fixture.TwoHop
  ( depThing
  ) where

import Dep.Facade
