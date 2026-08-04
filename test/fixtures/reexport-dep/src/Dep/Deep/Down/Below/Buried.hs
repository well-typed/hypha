-- | Five directories below the source root.
--
-- The predecessor of the cabal-driven enumeration walked four levels and
-- stopped, so a module at this depth was invisible — and indistinguishable
-- from one that did not exist.  @ghc-internal@ has fourteen of these,
-- including the @GHC.Internal.Control.Monad.ST.Lazy@ tree that declares
-- @runST@.
module Dep.Deep.Down.Below.Buried
  ( buried
  ) where

buried :: Int
buried = 5
