-- | An export no import of this module can account for.
--
-- @Fixture.Bystander@ is the only import and has no @depThing@; the
-- module that declares it is never named here at all.  Real sources
-- reach this shape through CPP, through a class method the parser cannot
-- see, and through re-export chains too long for one component's syntax
-- to describe.  Only the compiler knows, and it wrote the answer into the
-- interface file.
module Fixture.Blind
  ( depThing
  ) where

import Fixture.Bystander
