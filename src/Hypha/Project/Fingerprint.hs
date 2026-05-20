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

    visit :: FilePath -> FilePath -> IO [(FilePath, String, Integer)]
    visit parent name = do
      let p = parent </> name
      isDir <- doesDirectoryExist p
      if isDir
        then walkDir p
        else if takeExtension p `elem` [".hs", ".lhs"]
               then do
                 mt <- show <$> getModificationTime p
                 sz <- withFile p ReadMode hFileSize
                 pure [(p, mt, sz)]
               else pure []
