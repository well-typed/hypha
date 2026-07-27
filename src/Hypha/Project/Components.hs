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
import Language.Haskell.Extension qualified as Cabal
import GHC.Driver.Session qualified as GHCLang
import Hypha.Source.Extensions
  ( LanguageSettings (..), UnknownExtension, extensionFromFlagName )
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
  , ciOtherModules :: ![Text]
    -- ^ @other-modules@: present in the component, absent from its
    -- public surface.  The indexer needs them (their symbols are still
    -- searchable and still define re-exports) and search ranks them
    -- below the exposed ones.
  , ciLanguageSettings :: !LanguageSettings
    -- ^ @default-language@ + @default-extensions@, resolved to the form
    -- "Hypha.Source.Parser" wants.  Without these, a module that relies
    -- on a stanza-wide extension parses differently for us than for the
    -- compiler.
  , ciUnknownExtensions :: ![UnknownExtension]
    -- ^ @default-extensions@ entries GHC's flag table did not recognise.
    -- Carried rather than dropped: an unrecognised extension is a
    -- plausible cause of a downstream parse failure, and the indexer
    -- reports these alongside the failures they might explain.
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
          (on, off, unknown) = splitExtensions (PD.defaultExtensions bi)
      in ComponentInfo {
           ciKind         = kind
         , ciHsSourceDirs = dirs
         , ciExposedModules = map renderModule (PD.exposedModules lib)
         , ciOtherModules   = map renderModule (PD.otherModules bi)
         , ciLanguageSettings = LanguageSettings
             { lsLanguage   = ghcLanguageOf =<< PD.defaultLanguage bi
             , lsDefaultOn  = on
             , lsDefaultOff = off
             }
         , ciUnknownExtensions = unknown
         }

    renderModule = T.pack . render . pretty

    -- cabal models an extension as (name, enabled), and the name it
    -- carries can itself be negated (@NoImplicitPrelude@), so the two
    -- polarities compose by XNOR rather than conjunction: cabal's
    -- @DisableExtension ImplicitPrelude@ and an @EnableExtension
    -- (UnknownExtension \"NoImplicitPrelude\")@ must reach the same answer.
    splitExtensions exts =
      let resolved =
            [ (x, cabalOn == flagOn)
            | e <- exts
            , let (nm, cabalOn) = cabalExtensionName e
            , Right pairs <- [extensionFromFlagName nm]
            , (x, flagOn) <- pairs
            ]
          unknown =
            [ u
            | e <- exts
            , let (nm, _) = cabalExtensionName e
            , Left u <- [extensionFromFlagName nm]
            ]
      in ( [ x | (x, True)  <- resolved ]
         , [ x | (x, False) <- resolved ]
         , unknown
         )

    cabalExtensionName e = case e of
      Cabal.EnableExtension  k  -> (T.pack (show k), True)
      Cabal.DisableExtension k  -> (T.pack (show k), False)
      Cabal.UnknownExtension nm -> (T.pack nm, True)

-- | Translate cabal's @default-language@ into the parser's language
-- selector.  Cabal admits @UnknownLanguage@ for forward compatibility;
-- an unrecognised value means \"no opinion\", which leaves the GHC2021
-- floor in charge rather than inventing a language.
ghcLanguageOf :: Cabal.Language -> Maybe GHCLang.Language
ghcLanguageOf lang = case lang of
  Cabal.Haskell98         -> Just GHCLang.Haskell98
  Cabal.Haskell2010       -> Just GHCLang.Haskell2010
  Cabal.GHC2021           -> Just GHCLang.GHC2021
  Cabal.GHC2024           -> Just GHCLang.GHC2024
  Cabal.UnknownLanguage _ -> Nothing

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

