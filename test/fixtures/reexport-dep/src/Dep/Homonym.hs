-- | A private binding sharing a name with the one the chain is looking
-- for.
--
-- Mirrors the realistic hazard in @GHC.Internal.*@: short names like
-- @lines@ or @null@ have local helpers in modules that do not export
-- them.  A descent that takes \"declares it\" as \"defines it\" lands
-- here, because this module is reachable through an open import and the
-- real declaration is not.
module Dep.Homonym
  ( unrelated
  ) where

unrelated :: Bool
unrelated = True

-- | Same name, not exported.  Whoever finds this and stops is wrong.
depThing :: Int -> Int
depThing _ = 0
