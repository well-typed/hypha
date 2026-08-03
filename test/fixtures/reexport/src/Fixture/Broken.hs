-- | Deliberately unparsable.  Not listed in the cabal stanza, so it
-- never enters the index fixtures; used to pin that a parse failure is
-- reported as a failure rather than as "symbol not here".
module Fixture.Broken (f) where

f x = case x of
  -> 1
