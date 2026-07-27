{-# LANGUAGE BangPatterns #-}
-- | Strict counterpart of 'Fixture.Internal.insertBag'.  Same name, same
-- signature, different definition site — which is what the search
-- collapse rule has to keep apart.
module Fixture.StrictInternal
  ( insertBag
  ) where

import Fixture.Internal (Bag (..))

-- | Strict insert.
insertBag :: a -> Bag a -> Bag a
insertBag !x (Bag xs) = Bag (x : xs)
