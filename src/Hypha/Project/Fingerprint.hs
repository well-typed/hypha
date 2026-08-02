{-# LANGUAGE OverloadedStrings #-}
-- | Source-tree fingerprint used to detect when a component's bytes
-- have changed since the last index write.  The fingerprint is a
-- SHA-256 over the sorted list of @(absolute-path, mtime, size)@
-- triples for every @.hs@ / @.lhs@ file under the given source roots.
--
-- Determinism matters more than cryptographic strength; callers only
-- compare fingerprints for equality.  SHA-256 is already a transitive
-- dependency via @cryptohash-sha256@.
module Hypha.Project.Fingerprint
  ( componentFingerprint
  , hashParts
  ) where

import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import System.Directory
  ( doesDirectoryExist, getModificationTime, listDirectory )
import System.FilePath ((</>), takeExtension)
import System.IO (IOMode (ReadMode), hFileSize, withFile)

-- | A digest over an ordered list of parts, for callers whose inputs are
-- not a source tree.
--
-- Order is significant and the separator cannot occur in a part, so
-- @["a","bc"]@ and @["ab","c"]@ do not collide.
hashParts :: [Text] -> Text
hashParts parts =
  Text.decodeUtf8 (Base16.encode (SHA256.hash payload))
  where
    payload = BS.concat [ Text.encodeUtf8 p <> "\0" | p <- parts ]

-- | Compute a fingerprint over every Haskell source file under the
-- given roots.  Reordering the roots does not change the result.
-- Missing roots are silently skipped; the empty-input case still
-- produces a stable non-empty digest.
componentFingerprint :: [FilePath] -> IO Text
componentFingerprint roots = do
  triples <- concat <$> mapM walk roots
  let sorted = sort triples
      payload = BS.concat (map encodeTriple sorted)
      digest  = SHA256.hash payload
  pure (Text.decodeUtf8 (Base16.encode digest))
  where
    encodeTriple (p, mt, sz) = BS.concat
      [ Text.encodeUtf8 (Text.pack p)
      , "\0"
      , Text.encodeUtf8 (Text.pack mt)
      , "\0"
      , Text.encodeUtf8 (Text.pack (show sz))
      , "\n"
      ]

    walk :: FilePath -> IO [(FilePath, String, Integer)]
    walk root = do
      ok <- doesDirectoryExist root
      if not ok then pure [] else walkDir root

    walkDir :: FilePath -> IO [(FilePath, String, Integer)]
    walkDir d = do
      entries <- listDirectory d
      fmap concat $ mapM (visit d) entries

    -- Build output and VCS metadata are not the component's sources, and
    -- including them makes the fingerprint change for reasons that do not
    -- change a single row.  @dist-newstyle@ holds cabal's generated
    -- @Paths_pkg.hs@ and @PackageInfo_pkg.hs@, rewritten on every build,
    -- so a project package re-indexed after each @cabal build@; and this
    -- repo keeps agent checkouts under @.worktrees@, which tied one
    -- checkout's fingerprint to every other one.
    --
    -- Deliberately narrower than the indexer's skip list, which also
    -- drops @test@ and @bench@: over-invalidating costs a rebuild,
    -- under-invalidating serves stale rows.
    skipDir :: FilePath -> Bool
    skipDir name = name `elem`
      [ "dist", "dist-newstyle", ".stack-work", ".worktrees"
      , ".git", ".hypha", ".cabal-store"
      ]

    visit :: FilePath -> FilePath -> IO [(FilePath, String, Integer)]
    visit parent name = do
      let p = parent </> name
      isDir <- doesDirectoryExist p
      if isDir
        then if skipDir name then pure [] else walkDir p
        else if takeExtension p `elem` [".hs", ".lhs"]
               then do
                 mt <- show <$> getModificationTime p
                 sz <- withFile p ReadMode hFileSize
                 pure [(p, mt, sz)]
               else pure []
