-- | Declares a name that also exists in 'Fixture.Internal', so
-- definition-site resolution has two same-named definitions to keep
-- apart.
module Fixture.Other
  ( sizeBag
  , otherOnly
  ) where

-- | Same name as 'Fixture.Internal.sizeBag', different definition.
sizeBag :: [a] -> Int
sizeBag = length

-- | Unique to this module.
otherOnly :: Bool
otherOnly = True
