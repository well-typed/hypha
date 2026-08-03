-- | A class method exported by name, and a data type exported through
-- the @(..)@ wildcard.  Both were names the component exported and
-- declared nowhere: 'Hypha.Source.Parser.findDecl' saw top-level
-- declarations only, so the method, the constructors and the record
-- field had no declaration to read a signature from and got no index
-- row.  The pair used to vanish through a failing pattern guard.
module Fixture.Klass
  ( Klass (..)
  , klassMethod
  , Shape (..)
  ) where

class Klass a where
  klassMethod :: a -> Int

-- | A type whose members are reachable only through the wildcard.
data Shape
  = Circle
      { radius :: Int
      }
  | Square Int
