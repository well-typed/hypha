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
  , renderComponentKind
  , parseLibComponents
  , findCabalFile
  , getExposedModules
  ) where

import Control.Exception.Safe (IOException, displayException, try)
import Data.ByteString qualified as BS
import Data.Containers.ListUtils (nubOrd)
import Data.List (intercalate)
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
  ( CppEnv (..), LanguageSettings (..), UnknownExtension (..)
  , extensionFromFlagName )
import System.Directory (doesDirectoryExist, listDirectory)
import System.IO (hPutStrLn, stderr)
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

-- | The suffix a kind contributes to a component name: nothing for the
-- main library, @:name@ for a sub-library, @:exe:name@ for an executable.
-- Lives here, with the type, so the wire form has one definition.
renderComponentKind :: ComponentKind -> Text
renderComponentKind k = case k of
  MainLib  -> ""
  SubLib n -> ":" <> n
  Exe    n -> ":exe:" <> n

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
  :: FilePath        -- ^ cabal file path
  -> FilePath        -- ^ package root (for resolving relative source dirs)
  -> Maybe FilePath  -- ^ synthesised @cabal_macros.h@, when a plan supplied one
  -> IO [ComponentInfo]
parseLibComponents cabalPath pkgRoot mMacroHeader = do
  eBs <- try @IO @IOException (BS.readFile cabalPath)
  case eBs of
    Left err -> do
      report ("could not be read: " <> displayException err)
      pure []
    Right bs -> case PDP.parseGenericPackageDescriptionMaybe bs of
      Nothing  -> do
        report "is not a cabal file we can parse"
        pure []
      Just gpd ->
        let mainComp =
              [ toComponent MainLib (flattenCondTree ct)
              | ct <- maybe [] (:[]) (PD.condLibrary gpd)
              ]
            subComps =
              [ toComponent (SubLib (Text.pack (UC.unUnqualComponentName n))) (flattenCondTree ct)
              | (n, ct) <- PD.condSubLibraries gpd
              ]
            exeComps =
              [ toComponent (Exe (Text.pack (UC.unUnqualComponentName n)))
                  (PD.emptyLibrary { PD.libBuildInfo = PD.buildInfo (flattenCondTree ct) })
              | (n, ct) <- PD.condExecutables gpd
              ]
            comps    = mainComp ++ subComps ++ exeComps
        in do mapM_ reportUnknownExtensions comps
              pure comps
  where
    -- Neither failure is silent: a caller told only "no components"
    -- falls back to guessing, and the guess is what the component list
    -- exists to replace.
    report why = hPutStrLn stderr $
      "hypha: " <> cabalPath <> " " <> why
        <> "; its module lists and language settings are unavailable"

    -- An extension name cabal accepted and GHC's own table does not know.
    -- Rare, but it silently changes how we parse every module of the
    -- component, so it is said out loud rather than carried unread.
    reportUnknownExtensions ci = case ciUnknownExtensions ci of
      []   -> pure ()
      exts -> hPutStrLn stderr $
        "hypha: " <> cabalPath <> " sets default-extensions GHC does not"
          <> " recognise (" <> intercalate ", "
               [ T.unpack (unUnknownExtension e) | e <- exts ]
          <> "); modules of " <> T.unpack (describeKind (ciKind ci))
          <> " are parsed without them"

    describeKind k = case k of
      MainLib  -> "its library"
      SubLib n -> "its sub-library " <> n
      Exe    n -> "its executable " <> n

    toComponent kind lib =
      let bi   = PD.libBuildInfo lib
          raw  = nubOrd (map UP.getSymbolicPath (PD.hsSourceDirs bi))
          dirs = if null raw
                   then [pkgRoot]
                   else map (pkgRoot </>) raw
          (on, off, unknown) = splitExtensions (PD.defaultExtensions bi)
      in ComponentInfo {
           ciKind         = kind
         , ciHsSourceDirs = dirs
         , ciExposedModules = nubOrd (map renderModule (PD.exposedModules lib))
         , ciOtherModules   = nubOrd (map renderModule (PD.otherModules bi))
         , ciLanguageSettings = LanguageSettings
             { lsLanguage   = ghcLanguageOf =<< PD.defaultLanguage bi
             , lsDefaultOn  = on
             , lsDefaultOff = off
             , lsCpp        = CppEnv
                 { cppPreInclude  = mMacroHeader
                   -- The stanza's own @include-dirs@, plus the package
                   -- root and its source dirs: a module's @#include@ is
                   -- resolved against the including file's directory
                   -- already, but a header declared for the whole
                   -- package lives at one of these instead.
                 , cppIncludeDirs = nubOrd $
                     [ pkgRoot </> UP.getSymbolicPath p
                     | p <- PD.includeDirs bi ]
                     <> (pkgRoot : dirs)
                 }
             }
         , ciUnknownExtensions = unknown
         }

    renderModule = T.pack . render . pretty

    -- Every branch of a conditional stanza, unioned with the
    -- unconditional node.
    --
    -- @condTreeData@ alone is only the unconditional part, and cabal files
    -- put real module lists behind conditions: @base@ declares
    -- @GHC.Event@ solely in the @else@ of @if os(windows)@, and
    -- @System.CPUTime.Posix.*@ solely in the @else@ of an @elif@ chain.
    -- Reading only the unconditional node left those modules out of the
    -- component's list, and since the indexer treats a non-empty list as
    -- authoritative, they were never indexed at all.
    --
    -- The union is the right answer rather than resolving the flags: we
    -- cannot know the flag assignment the package was built with, and
    -- 'loadModuleSources' already drops a name whose file is not on disk,
    -- so a Windows-only module simply does not resolve on Linux.
    flattenCondTree :: Monoid a => PD.CondTree v c a -> a
    flattenCondTree ct =
      mconcat (PD.condTreeData ct : concatMap branch (PD.condTreeComponents ct))
      where
        branch b =
          flattenCondTree (PD.condBranchIfTrue b)
            : maybe [] (pure . flattenCondTree) (PD.condBranchIfFalse b)

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
      -- Only the module list is wanted here, which no macro affects.
      comps <- parseLibComponents fp root Nothing
      pure $ concatMap ciExposedModules comps

