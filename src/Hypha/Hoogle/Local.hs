{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
-- | Project-scoped Hoogle database lifecycle.
--
-- Bundles a single 'Hoogle.Database' path for
-- @\<project\>/.hypha/hoogle.hoo@.  The database is regenerated lazily
-- the first time 'ensureFresh' observes a stale stamp, and is guarded
-- by an 'MVar' so concurrent searches never spawn duplicate generation
-- work (the underlying @hoogle@ library deadlocks under concurrent
-- regen).
module Hypha.Hoogle.Local
  ( HyphaHoogle
  , openLocalHoogle
  , searchLocal
    -- * Internals exposed for tests + downstream wiring
  , scavengeStoreTxt
  ) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Data.List (isPrefixOf)
import qualified Data.Text as Text
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>))

import Hypha.Hoogle.Type (HoogleHit (..), HoogleQuery (..))
import Hypha.Types.PackageId
  ( PackageId (..), PackageName (..), Version (..) )

-- | Opaque handle to the local Hoogle DB lifecycle.
data HyphaHoogle = HyphaHoogle
  { hhDbPath    :: !FilePath
  , hhLock      :: !(MVar ())
  , hhStoreRoot :: !FilePath
    -- ^ @~/.cabal/store/ghc-X.Y.Z@ root for the active GHC.  Empty
    -- string when the store could not be located; scavenging then
    -- always returns 'Nothing' and the caller falls back to haddock.
  }

-- | Open (or initialise) the per-project Hoogle DB.  No regeneration
-- happens here; the first 'ensureFresh' triggers it lazily.
openLocalHoogle
  :: FilePath  -- ^ project @.hypha@ directory
  -> FilePath  -- ^ GHC store root (may be \"\" to disable scavenging)
  -> IO HyphaHoogle
openLocalHoogle dotHypha storeRoot = do
  lock <- newMVar ()
  pure HyphaHoogle
    { hhDbPath    = dotHypha </> "hoogle.hoo"
    , hhLock      = lock
    , hhStoreRoot = storeRoot
    }

-- | Locate @\<pkg\>.txt@ inside the cabal store.  Returns 'Nothing'
-- when the package is not installed with documentation.  The path
-- layout under the store is:
--
-- > <store-root>/<pkg-ver-hash>/share/doc/<pkg-ver>/html/<pkg>.txt
--
-- We list the @ghc-X.Y.Z@ root, pick hash dirs that begin with
-- @\<pkg\>-\<ver\>-@, and return the first one that carries the file.
scavengeStoreTxt :: FilePath -> PackageId -> IO (Maybe FilePath)
scavengeStoreTxt storeRoot pid = do
  rootOk <- doesDirectoryExist storeRoot
  if not rootOk then pure Nothing else do
    entries <- listDirectory storeRoot
    let prefix = Text.unpack (unPackageName (pkgName pid))
                 <> "-"
                 <> Text.unpack (unVersion (pkgVersion pid))
                 <> "-"
        candidates = [ storeRoot </> e | e <- entries, prefix `isPrefixOf` e ]
    firstHit candidates
  where
    firstHit []     = pure Nothing
    firstHit (c:cs) = do
      r <- probeDoc c
      case r of
        Just p  -> pure (Just p)
        Nothing -> firstHit cs

    probeDoc base = do
      let pkg = Text.unpack (unPackageName (pkgName pid))
          ver = Text.unpack (unVersion    (pkgVersion pid))
          path = base </> "share" </> "doc"
                      </> (pkg <> "-" <> ver)
                      </> "html" </> (pkg <> ".txt")
      ok <- doesFileExist path
      pure (if ok then Just path else Nothing)

-- | Stub: subsequent tasks fill in the generation lifecycle.  For
-- now, every query collapses to an empty result list.
searchLocal :: HyphaHoogle -> HoogleQuery -> IO [HoogleHit]
searchLocal hh _q = withMVar (hhLock hh) $ \_ -> pure []
