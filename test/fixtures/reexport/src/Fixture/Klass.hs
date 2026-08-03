-- | A class method exported by name.  It resolves to this module -- the
-- class is declared here -- and then 'findDecl', which sees top-level
-- declarations only, finds nothing called @klassMethod@.  The pair used
-- to vanish through a failing pattern guard.
module Fixture.Klass
  ( Klass (..)
  , klassMethod
  ) where

class Klass a where
  klassMethod :: a -> Int
