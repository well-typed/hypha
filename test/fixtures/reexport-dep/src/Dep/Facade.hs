-- | A facade /inside/ the dependency, so a re-export can take two hops:
-- the asking package reaches this module, and this module reaches the
-- declaration.  Mirrors @GHC.Internal.Data.List@, which exports
-- @mapAccumL@, imports it from @GHC.Internal.Data.Traversable@, and
-- declares nothing.
module Dep.Facade
  ( depThing
  ) where

import Dep.Internal (depThing)
