{-# LANGUAGE DerivingStrategies #-}
module Hypha.Project.Discovery
  ( -- * Types
    DiscoveryError (..)
    -- * Discovery
  , discoverProjectRoot
  ) where

import Control.Exception.Safe (IOException, try)
import Data.List (isSuffixOf)
import System.Directory
  ( canonicalizePath, doesFileExist, getCurrentDirectory, listDirectory )
import System.FilePath ((</>), takeDirectory)

import Hypha.Types.BuildPlan (ProjectRoot (..))

-- | Errors that can occur during project root discovery.
data DiscoveryError
  = NoProjectFound !FilePath
    -- ^ Walked to filesystem root without finding @cabal.project@ or @*.cabal@.
  deriving stock (Show, Eq)

-- | Walk up from the given directory (or CWD) looking for @cabal.project@
--   or any @*.cabal@ file.  Returns the first directory that contains one.
--
--   If a @cabal.project@ is found, that directory wins.
--   If only a @*.cabal@ file is found, that directory is used (implicit project).
--   If neither is found after reaching the filesystem root, returns
--   'NoProjectFound'.
discoverProjectRoot :: Maybe FilePath -> IO (Either DiscoveryError ProjectRoot)
discoverProjectRoot mDir = do
  startDir <- maybe getCurrentDirectory pure mDir
  canonical <- canonicalizePath startDir
  walkUp canonical
  where
    walkUp :: FilePath -> IO (Either DiscoveryError ProjectRoot)
    walkUp dir = do
      hasProject <- doesFileExist (dir </> "cabal.project")
      if hasProject
        then pure (Right (ProjectRoot dir))
        else do
          hasCabal <- hasAnyCabalFile dir
          if hasCabal
            then pure (Right (ProjectRoot dir))
            else let parent = takeDirectory dir
                 in if parent == dir
                    then pure (Left (NoProjectFound dir))
                    else walkUp parent

-- | Check if a directory contains any genuine @<pkg>.cabal@ file.
--
-- Real cabal package files look like @foo.cabal@: non-empty stem,
-- @.cabal@ suffix, and the entry must be a /file/.  The bare name
-- @.cabal@ does NOT qualify — that is the per-user cabal config
-- directory at @~/@, and historically a sibling check that only
-- looked at the suffix made 'discoverProjectRoot' mistake the home
-- directory for a cabal project.
hasAnyCabalFile :: FilePath -> IO Bool
hasAnyCabalFile dir = do
  result <- try @IO @IOException (listDirectory dir)
  case result of
    Left _        -> pure False
    Right entries ->
      anyM (\e -> doesFileExist (dir </> e))
           (filter looksLikeCabalFileName entries)
  where
    looksLikeCabalFileName name =
      name /= ".cabal" && ".cabal" `isSuffixOf` name

    anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
    anyM _ []     = pure False
    anyM f (x:xs) = do
      b <- f x
      if b then pure True else anyM f xs
