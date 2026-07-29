{-# LANGUAGE CPP #-}
-- | Mirrors OneTuple's @Data.Tuple.Solo.TH@: guarded on a macro only a
-- real GHC invocation defines.  @cpphs@ reports an @#error@ by calling
-- 'error' from pure code, so a caller that parses this module outside
-- 'Hypha.Source.Interface.parseInterfaceIO' does not get a @Left@ — it
-- gets an exception, which is how one module once aborted an entire index
-- pass and, later, answered a module page with a 500.
module Fixture.CppError (cppOnly) where

#ifndef CURRENT_PACKAGE_KEY
#error CURRENT_PACKAGE_KEY undefined
#endif

cppOnly :: Int
cppOnly = 0
