{-# LANGUAGE CPP                #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
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
  , ModuleRef (..)
  , expectedModuleName
  , renderComponentKind
  , parseLibComponents
  , findCabalFile
  , getExposedModules
  , evalCondition
  ) where

import Control.Exception.Safe (IOException, displayException, try)
import Data.ByteString qualified as BS
import Data.Containers.ListUtils (nubOrd)
import Data.List (intercalate)
import Data.Maybe (listToMaybe)
import Data.Text qualified as T
import Data.Text qualified as Text
import Data.Text (Text)
import Distribution.PackageDescription.Parsec qualified as PDP
import Distribution.PackageDescription qualified as PD
import Distribution.Pretty (pretty)
import Distribution.System (Platform (..))
import Distribution.Types.Condition (Condition (..))
import Distribution.Types.ConfVar (ConfVar (..))
import Distribution.Types.Executable qualified as PDE
import Distribution.Types.UnqualComponentName qualified as UC
import Distribution.Utils.Path qualified as UP
import Language.Haskell.Extension qualified as Cabal
import GHC.Driver.Session qualified as GHCLang
import Hypha.Project.BuildContext
  ( BuildContext (..), hostBuildContext, withIncludeDirs )
import Hypha.Source.Extensions
  ( LanguageSettings (..), UnknownExtension (..), extensionFromFlagName )
import Hypha.Types.SymbolPath (ModulePath (..), mainModulePath)
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

-- | How a cabal stanza names one of its modules.
--
-- @exposed-modules@ and @other-modules@ name a module by its dotted name,
-- which is resolved to a file under one of the @hs-source-dirs@.  An
-- executable's @main-is@ names a /file/, and that file need not be named
-- after a module at all: @main-is: regen-ngrams.hs@ is legal and
-- @regen-ngrams@ is not a module name.  Collapsing the two into one
-- 'Text' is what left every executable's main module out of its
-- component, so its page answered @module Main is not part of this
-- component@ and its symbols were never indexed.
data ModuleRef
  = ByName !ModulePath
    -- ^ Dotted, resolved against each @hs-source-dir@ in turn.
  | ByPath !FilePath
    -- ^ @main-is@, relative to an @hs-source-dir@.
  deriving stock (Show, Eq, Ord)

-- | The module name a ref is expected to carry.
--
-- Expected, not authoritative: the parse tree decides what a module is
-- called, and a disagreement is reported rather than assumed away.  A
-- @main-is@ file is expected to declare 'mainModulePath', which is both
-- GHC's default for an entry point and what a header-less script parses
-- as; a @-main-is@ rename shows up as the mismatch it is.
expectedModuleName :: ModuleRef -> ModulePath
expectedModuleName r = case r of
  ByName m -> m
  ByPath _ -> mainModulePath

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
  , ciMainIs :: !(Maybe FilePath)
    -- ^ An executable's @main-is@, relative to one of the
    -- @hs-source-dirs@; 'Nothing' for a library.  A path rather than a
    -- module name because that is what cabal accepts, and it lives here
    -- rather than being derived from the stanza's module lists because
    -- @BuildInfo@ does not carry it -- which is how it came to be
    -- dropped for every executable in the first place.
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
  -> BuildContext    -- ^ what the plan says: CPP environment + platform
  -> IO [ComponentInfo]
parseLibComponents cabalPath pkgRoot ctx = do
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
        let libComponent kind ct =
              let lib = flattenCondTree ct
              in toComponent kind Nothing (PD.exposedModules lib) (PD.libBuildInfo lib)
            mainComp =
              [ libComponent MainLib ct
              | ct <- maybe [] (:[]) (PD.condLibrary gpd)
              ]
            subComps =
              [ libComponent (SubLib (Text.pack (UC.unUnqualComponentName n))) ct
              | (n, ct) <- PD.condSubLibraries gpd
              ]
            -- Kept as a triple so the ambiguity report below can name the
            -- component it is about and the candidates it rejected.
            exeStanzas =
              [ (kind, nodes, mainIsCandidates nodes)
              | (n, ct) <- PD.condExecutables gpd
              , let kind  = Exe (Text.pack (UC.unUnqualComponentName n))
                    nodes = applicableNodes ct
              ]
            exeComps =
              [ toComponent kind (listToMaybe cands) []
                  (mconcat (map PD.buildInfo nodes))
              | (kind, nodes, cands) <- exeStanzas
              ]
            comps    = mainComp ++ subComps ++ exeComps
        in do mapM_ reportUnknownExtensions comps
              mapM_ reportAmbiguousMainIs exeStanzas
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

    -- An executable whose applicable branches name different main-is
    -- files.  Only an undecidable condition can produce this -- a
    -- @flag()@ or an @impl()@ we do not resolve -- and the first is
    -- taken, so which one it was has to be said out loud.
    reportAmbiguousMainIs (kind, _, cands) = case cands of
      (chosen : rest@(_ : _)) -> hPutStrLn stderr $
        "hypha: " <> cabalPath <> " names more than one main-is for "
          <> T.unpack (describeKind kind) <> " (" <> intercalate ", " cands
          <> "), because a flag() or impl() condition decides between them"
          <> "; using " <> chosen <> " and ignoring "
          <> intercalate ", " rest
      _ -> pure ()

    describeKind k = case k of
      MainLib  -> "its library"
      SubLib n -> "its sub-library " <> n
      Exe    n -> "its executable " <> n

    toComponent kind mMainIs exposed bi =
      let raw  = nubOrd (map UP.getSymbolicPath (PD.hsSourceDirs bi))
          dirs = if null raw
                   then [pkgRoot]
                   else map (pkgRoot </>) raw
          (on, off, unknown) = splitExtensions (PD.defaultExtensions bi)
      in ComponentInfo {
           ciKind         = kind
         , ciHsSourceDirs = dirs
         , ciExposedModules = nubOrd (map renderModule exposed)
         , ciOtherModules   = nubOrd (map renderModule (PD.otherModules bi))
         , ciMainIs         = mMainIs
         , ciLanguageSettings = LanguageSettings
             { lsLanguage   = ghcLanguageOf =<< PD.defaultLanguage bi
             , lsDefaultOn  = on
             , lsDefaultOff = off
               -- The stanza's own @include-dirs@, plus the package root
               -- and its source dirs, ahead of whatever the plan
               -- supplied (the compiler's own header directory): a
               -- module's @#include@ is resolved against the including
               -- file's directory already, but a header declared for the
               -- whole package lives at one of these instead, and a
               -- package's own header must shadow GHC's of the same name.
             , lsCpp        = bcCpp $ withIncludeDirs
                 (nubOrd $
                    [ pkgRoot </> UP.getSymbolicPath p
                    | p <- PD.includeDirs bi ]
                    <> (pkgRoot : dirs))
                 ctx
             }
         , ciUnknownExtensions = unknown
         }

    renderModule = T.pack . render . pretty

    -- The unconditional node, plus the branches that apply here.
    --
    -- @condTreeData@ alone is only the unconditional part, and cabal files
    -- put real module lists behind conditions: @base@ declares
    -- @GHC.Event@ solely in the @else@ of @if os(windows)@, and
    -- @System.CPUTime.Posix.*@ solely in the @else@ of an @elif@ chain.
    -- Reading only the unconditional node left those modules out of the
    -- component's list, and since the indexer treats a non-empty list as
    -- authoritative, they were never indexed at all.
    --
    -- A condition we can decide is decided: @os()@ and @arch()@ are
    -- answered by the plan's platform, so @base@ on Linux contributes
    -- @GHC.Event@ and not @GHC.Windows@.  Taking both branches there was
    -- not merely wasteful — @GHC.Windows@ /is/ on disk in the sdist, and
    -- it cannot preprocess off Windows (@WINDOWS_CCONV@ is defined only
    -- under @mingw32_HOST_OS@), so every descent through @base@ reported
    -- four parse failures for modules this platform never builds.
    --
    -- A condition we cannot decide is still unioned: the flag assignment
    -- the package was built with is not in the plan, and @impl()@ ranges
    -- over compilers we are not asked about.  Under-reading a module list
    -- costs an absent symbol; over-reading one costs a file that
    -- 'loadModuleSources' drops when it is not on disk.
    applicableNodes :: PD.CondTree ConfVar c a -> [a]
    applicableNodes ct =
      PD.condTreeData ct : concatMap branch (PD.condTreeComponents ct)
      where
        branch b = case evalCondition (bcPlatform ctx) (PD.condBranchCondition b) of
          Just True  -> applicableNodes (PD.condBranchIfTrue b)
          Just False -> concatMap applicableNodes
                          (maybe [] (: []) (PD.condBranchIfFalse b))
          Nothing    ->
            applicableNodes (PD.condBranchIfTrue b)
              <> concatMap applicableNodes
                   (maybe [] (: []) (PD.condBranchIfFalse b))

    flattenCondTree :: Monoid a => PD.CondTree ConfVar c a -> a
    flattenCondTree = mconcat . applicableNodes

    -- | Every @main-is@ the applicable branches name, deduplicated and in
    -- tree order.
    --
    -- Read off each node rather than taken from @mconcat@ of the
    -- 'PD.Executable's: cabal's own 'Semigroup' for that type calls
    -- 'error' when two of them disagree on @main-is@, and an undecidable
    -- @flag()@ or @impl()@ leaves us unioning branches that legitimately
    -- do.  Reading the field keeps a disagreement a value we can report.
    mainIsCandidates nodes =
      nubOrd [ p | e <- nodes, let p = mainIsOf e, not (null p) ]

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

-- | An executable's @main-is@, as a path relative to one of its
-- @hs-source-dirs@.
--
-- CPP because the field's type is not stable across the @Cabal-syntax@
-- versions this package supports: a bare 'FilePath' up to 3.12 (GHC
-- 9.6, 9.10), a @RelativePath Source File@ from 3.14 (GHC 9.12).  There
-- is no accessor common to both, and the alternative -- dropping the
-- field -- is the bug this exists to fix.
mainIsOf :: PD.Executable -> FilePath
#if MIN_VERSION_Cabal_syntax(3,14,0)
mainIsOf = UP.getSymbolicPath . PDE.modulePath
#else
mainIsOf = PDE.modulePath
#endif

-- | Decide a cabal condition as far as the plan's platform allows.
--
-- Three-valued on purpose.  @os()@ and @arch()@ are facts the plan
-- settles; @flag()@ and @impl()@ are not, and guessing either would
-- silently pick a module list the package was never built with.
-- 'Nothing' means "undecided", and the caller unions both branches for
-- those — the behaviour every branch used to get.
--
-- The connectives are Kleene's, so a decidable half still decides the
-- whole: @os(windows) && flag(x)@ is 'Just' 'False' off Windows even
-- though the flag is unknown.
evalCondition :: Platform -> Condition ConfVar -> Maybe Bool
evalCondition (Platform arch os) = go
  where
    go = \case
      Var (OS o)    -> Just (o == os)
      Var (Arch a)  -> Just (a == arch)
      Var (PackageFlag _) -> Nothing
      Var (Impl _ _)  -> Nothing
      Lit b         -> Just b
      CNot c        -> not <$> go c
      CAnd a b      -> kleeneAnd (go a) (go b)
      COr  a b      -> kleeneOr  (go a) (go b)

    -- One 'False' settles a conjunction whatever the other half is;
    -- one 'True' settles a disjunction.  Keeps a decidable @os()@ from
    -- being lost to an undecidable @flag()@ beside it.
    kleeneAnd (Just False) _ = Just False
    kleeneAnd _ (Just False) = Just False
    kleeneAnd a b            = (&&) <$> a <*> b

    kleeneOr (Just True) _ = Just True
    kleeneOr _ (Just True) = Just True
    kleeneOr a b           = (||) <$> a <*> b

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
      -- Only the module list is wanted here, which no macro affects —
      -- but the platform does, so the host's is used rather than none.
      comps <- parseLibComponents fp root hostBuildContext
      pure $ concatMap ciExposedModules comps

