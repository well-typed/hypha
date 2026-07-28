-- | A pure facade over another package.  The @base@ shape since GHC 9.10:
-- an explicit export list, one open import, and nothing declared here.
module Fixture.Imported
  ( depThing
  ) where

import Dep.Internal
