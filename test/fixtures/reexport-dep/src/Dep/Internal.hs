-- | Definition site in another package.  Mirrors
-- @GHC.Internal.Data.Traversable@, which is where @base@'s
-- @Data.Traversable@ exports actually live.
module Dep.Internal
  ( depThing
  , depUnused
  ) where

-- | Re-exported by @Fixture.Imported@ in the neighbouring fixture package.
depThing :: Int -> Int
depThing n = n + 1

-- | Exported here and by nobody else, so a cross-package pass cannot claim
-- it just because it is in scope.
depUnused :: Bool
depUnused = True
