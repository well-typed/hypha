-- | Declares @Fixture.Declared@ although it lives at
-- @Fixture/Renamed.hs@.  The parse tree is the authority on a module's
-- name; the path is only evidence.
module Fixture.Declared (declaredHere) where

declaredHere :: Int
declaredHere = 1
