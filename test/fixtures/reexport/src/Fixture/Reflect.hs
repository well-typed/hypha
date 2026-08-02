-- | Re-exports a module the component does not have, the shape mtl's
-- @Control.Monad.State@ uses when it exports @module Control.Monad@.
-- Its names cannot be expanded here, so the loss has to be reported.
module Fixture.Reflect
  ( module Data.List
  , localThing
  ) where

import Data.List

localThing :: Int
localThing = 1
