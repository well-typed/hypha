-- | Needs @MagicHash@ to parse and says so nowhere: the extension comes
-- from the component's @default-extensions@, which is the only reason
-- this module is readable at all.  A pass that parses it under the
-- GHC2021 floor instead reports it unparseable — and a pass that resolves
-- a symbol with the stanza's settings and then re-parses without them
-- reports the symbol missing from the module it just found it in.
module Fixture.Unboxed (unboxedAdd) where

import GHC.Exts (Int#, (+#))

unboxedAdd :: Int# -> Int#
unboxedAdd x = x +# 1#
