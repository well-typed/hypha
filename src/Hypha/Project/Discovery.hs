{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Project.Discovery
  ( -- * Types
    DiscoveryError (..)
    -- * Discovery
  , discoverProjectRoot
  , cabalStoreBase
  ) where

import Control.Exception.Safe (IOException, try)
import Data.List (isSuffixOf)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Maybe (listToMaybe, mapMaybe)
import qualified Distribution.Fields as Fields
import System.Directory
  ( canonicalizePath, doesFileExist, getCurrentDirectory, getHomeDirectory
  , listDirectory )
import System.Environment (lookupEnv)
import System.FilePath ((</>), takeDirectory)
import System.IO (hPutStrLn, stderr)

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

-- | The cabal store this project builds into: the directory whose
-- @ghc-X.Y.Z@ children hold the installed packages.
--
-- Resolved the way cabal resolves it.  A @store-dir@ in
-- @cabal.project.local@ beats one in @cabal.project@; a relative value
-- is taken against the project root.  Without a project, or without
-- the field, @$CABAL_DIR/store@ and then @~/.cabal/store@.
--
-- @import:@ed project files are not followed.
cabalStoreBase :: Maybe ProjectRoot -> IO FilePath
cabalStoreBase mRoot = do
  fromProject <- maybe (pure Nothing) storeDirField mRoot
  case fromProject of
    Just dir -> pure dir
    Nothing  -> do
      mCabalDir <- lookupEnv "CABAL_DIR"
      home      <- getHomeDirectory
      pure (maybe (home </> ".cabal" </> "store") (</> "store") mCabalDir)
  where
    -- Later files win, as they do for cabal itself.
    storeDirField (ProjectRoot root) =
      fmap (fmap (root </>) . listToMaybe . reverse . concat) $
        mapM (fieldIn . (root </>)) ["cabal.project", "cabal.project.local"]

    -- The project file has the same field syntax as a @.cabal@ file, so
    -- the same lexer reads it.  A file cabal would refuse is reported,
    -- not treated as having no @store-dir@.
    fieldIn file = do
      exists <- doesFileExist file
      if not exists
        then pure []
        else do
          parsed <- Fields.readFields <$> BS.readFile file
          case parsed of
            Right fields -> pure (mapMaybe storeDirOf fields)
            Left err     -> do
              hPutStrLn stderr ("warning: " <> file <> ": " <> show err)
              pure []

    -- Top level only: a field inside a @package@ section is not ours,
    -- and @store-dir@ is not valid there anyway.
    storeDirOf (Fields.Field (Fields.Name _ "store-dir") ls) =
      case BS8.unpack (BS8.unwords [ v | Fields.FieldLine _ v <- ls ]) of
        ""  -> Nothing
        dir -> Just dir
    storeDirOf _ = Nothing
