{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
-- | Read a package's @.cabal@ file and report the library components
-- it defines (main library + every @library NAME@ stanza), together
-- with the absolute paths to their @hs-source-dirs@.
--
-- This is the entry point for sub-library indexing in @hypha server@:
-- each component becomes its own browsable entry under @pkg:sublib@.
-- Failures (parse error, missing file) collapse to an empty list — the
-- indexer falls back to its heuristic source-root walk in that case.
module Hypha.Project.Components
  ( ComponentInfo (..)
  , parseLibComponents
  , findCabalFile
  ) where

import Control.Exception (IOException, try)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Distribution.PackageDescription as PD
import qualified Distribution.PackageDescription.Parsec as PDP
import qualified Distribution.Types.UnqualComponentName as UC
import qualified Distribution.Utils.Path as UP
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>), takeExtension)

-- | One library component of a package.
data ComponentInfo = ComponentInfo
  { ciSublib       :: !(Maybe Text)
    -- ^ 'Nothing' for the main library; 'Just' for a sub-library.
  , ciHsSourceDirs :: ![FilePath]
    -- ^ Absolute paths.  Falls back to the package root when the
    -- stanza omits @hs-source-dirs@ (cabal default).
  }
  deriving stock (Show, Eq)

-- | Locate the @.cabal@ file inside a package source directory.  Cabal
-- forbids more than one, so the first match is canonical.
findCabalFile :: FilePath -> IO (Maybe FilePath)
findCabalFile dir = do
  ok <- doesDirectoryExist dir
  if not ok
    then pure Nothing
    else do
      entries <- listDirectory dir
      pure $ case filter ((== ".cabal") . takeExtension) entries of
        (f : _) -> Just (dir </> f)
        []      -> Nothing

-- | Parse a @.cabal@ file and return one 'ComponentInfo' per library
-- component (main + sublibs).  Returns @[]@ on parse failure or
-- missing file.
parseLibComponents
  :: FilePath  -- ^ cabal file path
  -> FilePath  -- ^ package root (for resolving relative source dirs)
  -> IO [ComponentInfo]
parseLibComponents cabalPath pkgRoot = do
  eBs <- try @IOException (BS.readFile cabalPath)
  case eBs of
    Left _   -> pure []
    Right bs -> case PDP.parseGenericPackageDescriptionMaybe bs of
      Nothing  -> pure []
      Just gpd ->
        let mainComp =
              [ toComponent Nothing
                  (PD.libBuildInfo (PD.condTreeData ct))
              | ct <- maybe [] (:[]) (PD.condLibrary gpd)
              ]
            subComps =
              [ toComponent (Just (Text.pack (UC.unUnqualComponentName n)))
                  (PD.libBuildInfo (PD.condTreeData ct))
              | (n, ct) <- PD.condSubLibraries gpd
              ]
        in pure (mainComp ++ subComps)
  where
    toComponent name bi =
      let raw  = map UP.getSymbolicPath (PD.hsSourceDirs bi)
          dirs = if null raw
                   then [pkgRoot]
                   else map (pkgRoot </>) raw
      in ComponentInfo name dirs
