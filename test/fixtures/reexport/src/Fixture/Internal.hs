{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RoleAnnotations #-}
-- | Definition site for the fixture package.  Mirrors the shapes that
-- broke the real indexer: a role annotation, an unboxed primitive, and
-- CPP.
module Fixture.Internal
  ( Bag (..)
  , insertBag
  , sizeBag
  , internalOnly
  ) where

import GHC.Exts (Int (..), (+#))

-- | A bag of values.
data Bag a = Bag ![a]

type role Bag nominal

-- | Insert into the bag.
insertBag :: a -> Bag a -> Bag a
insertBag x (Bag xs) = Bag (x : xs)

-- | Size of the bag, via an unboxed add so MagicHash is load-bearing.
sizeBag :: Bag a -> Int
sizeBag (Bag xs) = case length xs of
  I# n -> I# (n +# 0#)

-- | Not exported by any wrapper; stays Internal-only.
internalOnly :: Bag a -> Bool
internalOnly (Bag xs) = null xs
