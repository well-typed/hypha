-- | A stray script under the source dir.  It is not in the cabal
-- stanza's module lists and must never be indexed as a module — the
-- path-walking indexer turned files like this into rows named
-- @concasync@ and @race@.
main :: IO ()
main = putStrLn "not a library module"
