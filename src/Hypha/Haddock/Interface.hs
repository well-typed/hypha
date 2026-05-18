module Hypha.Haddock.Interface
  ( hasInterfaceFile
  ) where

import Hypha.Types.PackageId (PackageId)

-- | Check whether a compiled @.haddock@ interface file exists for the
-- given package.  This is a stub: the real implementation would look in
-- the cabal store's @lib/<ghc>/<pkg-id>@ directory for @<pkg-name>.haddock@.
--
-- Post-MVP: wire to 'BuildEnv.locateHaddockHtml' or a dedicated
-- 'locateHaddockInterface' field.
hasInterfaceFile :: PackageId -> IO Bool
hasInterfaceFile _ = pure False
