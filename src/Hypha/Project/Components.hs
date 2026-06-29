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
  , ComponentKind (..)
  , parseLibComponents
  , findCabalFile
  , getExposedModules
  ) where

import Control.Exception.Safe (IOException, try)
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Text qualified as Text
import Data.Text (Text)
import Distribution.PackageDescription.Parsec qualified as PDP
import Distribution.PackageDescription qualified as PD
import Distribution.Pretty (pretty)
import Distribution.Types.UnqualComponentName qualified as UC
import Distribution.Utils.Path qualified as UP
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>), takeExtension)
import Text.PrettyPrint (render)

-- | The kind of library or executable component we discovered in a
-- cabal file.  'MainLib' represents the unnamed @library@ stanza;
-- 'SubLib' is a named @library NAME@ stanza; 'Exe' is an
-- @executable NAME@ stanza.
data ComponentKind
  = MainLib
  | SubLib !Text
  | Exe    !Text
  deriving stock (Show, Eq, Ord)

-- | One library or executable component of a package.
data ComponentInfo = ComponentInfo
  { ciKind         :: !ComponentKind
  , ciHsSourceDirs :: ![FilePath]
    -- ^ Absolute paths.  Falls back to the package root when the
    -- stanza omits @hs-source-dirs@ (cabal default).
  , ciExposedModules :: ![Text]
    -- ^ The textual rendition of modules exposed by this library
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
  eBs <- try @IO @IOException (BS.readFile cabalPath)
  case eBs of
    Left _   -> pure []
    Right bs -> case PDP.parseGenericPackageDescriptionMaybe bs of
      Nothing  -> pure []
      Just gpd ->
        let mainComp =
              [ toComponent MainLib (PD.condTreeData ct)
              | ct <- maybe [] (:[]) (PD.condLibrary gpd)
              ]
            subComps =
              [ toComponent (SubLib (Text.pack (UC.unUnqualComponentName n))) (PD.condTreeData ct)
              | (n, ct) <- PD.condSubLibraries gpd
              ]
            exeComps =
              [ toComponent (Exe (Text.pack (UC.unUnqualComponentName n)))
                  (PD.emptyLibrary { PD.libBuildInfo = (PD.buildInfo (PD.condTreeData ct)) })
              | (n, ct) <- PD.condExecutables gpd
              ]
        in pure (mainComp ++ subComps ++ exeComps)
  where
    toComponent kind lib =
      let bi   = PD.libBuildInfo lib
          raw  = map UP.getSymbolicPath (PD.hsSourceDirs bi)
          dirs = if null raw
                   then [pkgRoot]
                   else map (pkgRoot </>) raw
      in ComponentInfo {
           ciKind         = kind
         , ciHsSourceDirs = dirs
         , ciExposedModules = map (T.pack . render . pretty) $ PD.exposedModules lib
         }

-- | Get /ALL/ the exposed modules from a package source directory. This returns
-- the list of all the modules for all the stanzas.
getExposedModules :: FilePath -> IO [Text]
getExposedModules root = do
  mCabal <- findCabalFile root
  case mCabal of
    Nothing  -> pure []
    Just fp  -> do
      comps <- parseLibComponents fp root
      pure $ concatMap ciExposedModules comps

