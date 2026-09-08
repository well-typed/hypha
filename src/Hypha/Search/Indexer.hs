{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Building the search index: walk a component's modules, extract rows,
-- persist them, publish them into the in-memory scorer.
--
-- Split from "Hypha.Search.Index" because the row types have to sit
-- /below/ the cache (the cache knows about rows; rows know nothing about
-- storage) while the builder sits /above/ it (it reads and writes through
-- the cache).  One module for both would be a dependency cycle.
--
-- Lifted out of "Hypha.Command.Server", which owned both the HTTP wiring
-- and the indexer at 843 lines.
module Hypha.Search.Indexer
  ( -- * Building
    buildAndCacheIndex
  , Hydrated (..)
  , hydrateFromCache
    -- * The pure core
  , ComponentIndex (..)
  , OutsideExport (..)
  , indexComponentPure
  , repairUnresolved
  , componentScorerRows
    -- * Component discovery
  , componentModules
  , componentKindsOf
  , languageSettingsFor
  , indexInputsFingerprint
  , packageSources
  , stanzaModules
  , enumModulesIn
  , enumModuleFilesIn
  , chooseSourceRoots
  ) where

import Control.Exception qualified as Exception
import Control.Monad (filterM, foldM, void)
import Data.IORef qualified as IORef
import Data.Map.Strict qualified as Map
import Data.List (intercalate, sort, sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (catMaybes, fromMaybe, isJust, listToMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import System.Directory qualified as Dir
import System.FilePath qualified as FP
import System.IO (hPutStrLn, stderr)

import Hypha.Encoding (readSourceFile)
import Hypha.Error (errorMessage)
import Hypha.Package.Resolver (PackageResolver (..))
import Hypha.Project.BuildContext (BuildContext)
import Hypha.Project.Fingerprint qualified as Fingerprint
import Hypha.Project.Components qualified as Comp
import Hypha.Search.Fuzzy qualified as Fuzzy
import Hypha.Search.Exports
  (Export (..), ExportChoice (..), ExportEnv, lookupExport)
import Hypha.Search.Exports qualified as Exports
import Hypha.Search.Index
  (DefinitionRef (..), IndexRow (..), ModuleSource (..), Visibility (..))
import Hypha.Search.Reexport (DefinitionSite (..))
import Hypha.Search.Reexport qualified as Reexport
import Hypha.Source.Extensions (LanguageSettings)
import Hypha.Source.Extensions qualified as Extensions
import Hypha.Source.Interface (ModuleInterface (..))
import Hypha.Source.Interface qualified as Interface
import Hypha.Source.Origins qualified as Origins
import Hypha.Search.PackageCache (CacheOrigin (..))
import Hypha.Search.PackageCache qualified as Cache
import Hypha.Source.Parser qualified as Parser
import Hypha.Types.BuildPlan
import Hypha.Types.ComponentName (ComponentKey (..), componentKeyOf)
import Hypha.Types.PackageId
import Hypha.Types.SymbolPath (ModulePath (..), Signature (..), SymbolName (..))

-- | Module-name enumeration over an explicit list of source roots.
enumModulesIn :: [FilePath] -> IO [Text]
enumModulesIn = fmap (map (unModulePath . fst)) . enumModuleFilesIn

-- | 'enumModulesIn', keeping the file each name was derived from.
--
-- The dependency-closure walk needs both halves and the indexer needs
-- only the first; deriving the path a second time from the module name
-- would reintroduce exactly the @hs-source-dirs@ guesswork the walk did
-- for us.
enumModuleFilesIn :: [FilePath] -> IO [(ModulePath, FilePath)]
enumModuleFilesIn roots = concat <$> mapM walk roots
  where
    walk r = do
      files <- findHs r 4
      pure [ (modulePathOf r f, f) | f <- files ]

    modulePathOf r f =
      ModulePath (Text.pack (hsToModule (drop (length r + 1) f)))

-- | Enumerate every component of a unit (main + sublibs + exes) as
-- @(kind, sourceDirs)@ pairs.  Falls back to a single fallback entry
-- using the heuristic root walk when the unit has no parsed
-- components.
componentsForUnit
  :: BuildPlan -> PackageId -> FilePath
  -> IO [(Comp.ComponentKind, [FilePath])]
componentsForUnit plan pid d = case componentsOf plan pid of
  Just cs -> pure [ (Comp.ciKind c, Comp.ciHsSourceDirs c) | c <- cs ]
  Nothing -> do
    roots <- chooseSourceRoots d
    pure [(Comp.MainLib, roots)]

-- | The components the plan records for a unit, or 'Nothing' when it
-- records none — every store dependency, whose @pkg-src@ we never parsed.
--
-- One derivation, because the build pass and the hydrate pass are two
-- halves of the same decision: they must agree on which component keys a
-- unit has, or hydration looks for keys the build never wrote.
componentsOf :: BuildPlan -> PackageId -> Maybe [Comp.ComponentInfo]
componentsOf plan pid = case lookupUnit (pkgName pid) plan of
  Just pu | not (null (puLibComponents pu)) -> Just (puLibComponents pu)
  _                                         -> Nothing

-- | The component kinds a unit has, mirroring 'componentsForUnit' without
-- needing a source directory: hydration works off the cache alone.
componentKindsOf :: BuildPlan -> PackageId -> NonEmpty Comp.ComponentKind
componentKindsOf plan pid = case componentsOf plan pid of
  Just (c : cs) -> Comp.ciKind c :| map Comp.ciKind cs
  _             -> Comp.MainLib :| []

-- | What hydration recovered: the exports of every component it loaded,
-- and the units it could not.
--
-- The environment is returned rather than rebuilt later because the
-- background pass needs it before it indexes anything: a warm cache
-- holding @ghc-internal@ is exactly how @base@ becomes resolvable in a run
-- that only rebuilds @base@.
data Hydrated = Hydrated
  { hyEnv     :: !ExportEnv
  , hyMissing :: ![PackageId]
  }

-- | Pull every cached component index into the in-memory ref.  A unit
-- counts as "fully hydrated" only when /every/ one of its components
-- has cached rows; otherwise it's reported as missing so the
-- background indexer rebuilds the whole set.
hydrateFromCache
  :: BuildPlan
  -> Cache.HyphaPackageCache
  -> [PackageId]
  -> IORef.IORef [Fuzzy.IndexedRow]
  -> IO Hydrated
hydrateFromCache plan cache pids ref = go Exports.emptyEnv [] pids
  where
    go env missing [] = pure Hydrated
      { hyEnv     = env
      , hyMissing = reverse missing
      }
    go env missing (pid : rest) = do
      let verT  = unVersion (pkgVersion pid)
          unit  = unitIdFor plan pid
          kinds = NE.toList (componentKindsOf plan pid)
          keys  = [ unComponentKey (componentKeyOf (pkgName pid) k) | k <- kinds ]
      -- The same digest the build pass stamped, read from the same
      -- configuration it was stamped under.  A component whose inputs
      -- moved -- a source edit, a new compiler, a cabal file we can read
      -- this time and could not last time -- reports as missing and is
      -- rebuilt, which is the whole point.
      hits <- mapM (freshFor verT unit pid) kinds
      if and hits
        then do
          env' <- foldM (loadKey verT unit) env keys
          -- Once per unit, not once per component key.
          publishRows ref [Fuzzy.mkPackageRow (pkgName pid) (pkgVersion pid)]
          go env' missing rest
        else go env (pid : missing) rest

    freshFor verT unit pid kind = do
      fp <- indexInputsFingerprint plan pid kind
      Cache.haveFreshIndex cache
        (unComponentKey (componentKeyOf (pkgName pid) kind)) verT unit fp

    loadKey verT unit env k = do
      rows <- Cache.readCachedIndex cache k verT unit
      publishRows ref (componentScorerRows rows)
      pure (Exports.extendEnv rows env)


-- | Walk the source trees of the given packages, extract their module
-- exports, persist the result to the cache, and prepend them to the
-- in-memory ref.  Packages whose source cannot be resolved are skipped,
-- with a reason on stderr — the index is a best-effort fallback, not a
-- silent one.
--
-- Units are walked dependencies-first and each component's rows extend the
-- environment the next one resolves against.  That order is a correctness
-- requirement, not a performance one: @base@ has no signature for
-- @mapAccumL@ until @ghc-internal@ has been indexed.
--
-- Per-module rows are built fully /outside/ the atomicModifyIORef'
-- critical section; prepending makes each insert O(|rows|) instead of
-- the O(|index|) behaviour of @old ++ rows@.
buildAndCacheIndex
  :: BuildPlan
  -> Cache.HyphaPackageCache
  -> PackageResolver IO
  -> Maybe (Origins.OriginOracle IO)
     -- ^ Where the compiler says exports come from, when there is a
     -- compiler to ask.  'Nothing' means the caller already reported why
     -- there is not — repeating it once per module would bury every other
     -- diagnostic.
  -> ExportEnv                          -- ^ what the warm cache already supplies
  -> [PackageId]
  -> IORef.IORef [Fuzzy.IndexedRow]
  -> IORef.IORef Int                    -- ^ packages-done counter
  -> IO ()
buildAndCacheIndex plan cache resolver oracle env0 pids ref doneRef =
  void (foldM indexUnit env0 (topologicalOrder plan pids))
  where
    -- Local + source-repository-package units land in the project DB;
    -- everything else (store packages) goes to the shared global DB.
    originFor :: PackageId -> CacheOrigin
    originFor pid = case lookupUnit (pkgName pid) plan of
      Just u | puIsLocal u -> OriginProject
      _                    -> OriginGlobal
    -- The done counter bumps once per /unit/, not per component, so
    -- the progress bar continues to read in package units.
    bump = IORef.atomicModifyIORef' doneRef (\n -> (n + 1, ()))

    -- One unit, fenced: whatever goes wrong inside it — a source the
    -- resolver cannot produce, a file that will not read, a parser
    -- blowing up — is that unit's problem.  Reported and skipped, so the
    -- packages after it in the build order still get indexed rather than
    -- silently missing while the server reports itself ready.
    indexUnit env pid = do
      r <- Exception.try (indexUnitUnfenced env pid)
      case r of
        Right env' -> pure env'
        -- The wrapped exception, not the 'SomeException': displaying the
        -- wrapper appends GHC's backtrace, which is noise on a line the
        -- user reads to learn which package went missing and why.
        Left (Exception.SomeException e) -> do
          skip pid (Text.pack (Exception.displayException e))
          pure env

    indexUnitUnfenced env pid = do
      eDir <- resolveSrc resolver pid
      case eDir of
        Left err -> do
          skip pid ("no source: " <> errorMessage err)
          pure env
        Right d -> do
          comps <- componentsForUnit plan pid d
          env'  <- foldM (indexComponent pid) env comps
          publishRows ref [Fuzzy.mkPackageRow (pkgName pid) (pkgVersion pid)]
          bump
          pure env'

    skip pid why = do
      hPutStrLn stderr $
        "hypha index: skipping " <> Text.unpack (unPackageName (pkgName pid))
          <> ": " <> Text.unpack why
      bump

    indexComponent pid env (kind, srcDirs) = do
      let pkgT    = unPackageName (pkgName    pid)
          verT    = unVersion    (pkgVersion pid)
          compKey = componentKeyOf (PackageName pkgT) kind
          langs   = languageSettingsFor plan (pkgName pid) kind
          deps    = dependencySet plan pid
      sources <- componentModules plan pid kind srcDirs
      parsed   <- Interface.parseSources langs sources
      -- What syntax could resolve, then what only the compiler knows.
      let syntactic = indexParsedComponent compKey deps env parsed
      ci <- maybe (pure syntactic)
                  (\o -> repairUnresolved o compKey pid deps env syntactic)
                  oracle
      let flatRows = ciRows ci
      reportComponentIndex compKey ci
      -- Persist before publishing into memory so a crash mid-stream
      -- never leaves the in-memory view ahead of the cache.
      Cache.writeCachedIndex cache (originFor pid)
        (unComponentKey compKey) verT (unitIdFor plan pid) flatRows
      -- After the rows: 'writeIndex' replaces the meta row, so stamping
      -- the fingerprint first would lose it and the component would
      -- rebuild on every start.
      fp <- indexInputsFingerprint plan pid kind
      Cache.writeCachedFingerprint cache (originFor pid)
        (unComponentKey compKey) verT (unitIdFor plan pid) fp
      publishRows ref (componentScorerRows flatRows)
      pure (Exports.extendEnv flatRows env)

-- | The configuration to file a package's rows under.
--
-- A package the plan does not mention cannot be keyed by a configuration
-- cabal resolved, because there is none; it is keyed by its version
-- instead, the same way an override is, so its rows stay findable and
-- stay out of every plan-scoped read.  Reachable only through a resolver
-- that answered for a package outside the plan.
unitIdFor :: BuildPlan -> PackageId -> UnitId
unitIdFor plan pid =
  fromMaybe (unpinnedUnitIdFor pid) (lookupUnitId (pkgName pid) plan)

-- | Everything that decides what rows a component produces, as one digest.
--
-- Cache warmth used to mean "a meta row exists for this @(pkg, version)@",
-- which is wrong in three ways at once.  A local package keeps its version
-- across every edit, so the project's own symbols froze after the first
-- indexing run — hypha could not find a function you had just written.  A
-- component that produced no rows because its cabal stanzas were not yet
-- readable counted as warm forever, so the degraded answer was the final
-- one.  And the global DB is shared across projects while the rows depend
-- on the compiler and the resolved language settings.
--
-- So the fingerprint covers: the source bytes (for units whose directory
-- the plan knows without resolving anything), the compiler, the language
-- settings, whether the cabal component list was available, and the
-- resolved dependency versions.
--
-- A store package's own bytes are immutable at a given version, and
-- walking its tree would mean extracting every dependency at startup, so
-- for those the source term is the version alone.
indexInputsFingerprint
  :: BuildPlan -> PackageId -> Comp.ComponentKind -> IO Text
indexInputsFingerprint plan pid kind = do
  srcFp <- mutableSourceFingerprint plan (pkgName pid)
  depFps <- mapM (mutableSourceFingerprint plan . pkgName) deps
  pure $ Fingerprint.hashParts $ concat
    -- v2: rows now come from probing every candidate import and from the
    -- compiler's own export origins, so a v1 component is missing rows a
    -- rebuild would produce.
    [ [ "v2", srcFp, unCompilerId (bpCompiler plan) ]
    , [ unVersion (pkgVersion pid) ]
    , [ Comp.renderComponentKind kind ]
    , languageFingerprint (languageSettingsFor plan (pkgName pid) kind)
    , [ if isJust (componentsOf plan pid) then "cabal" else "walked" ]
      -- A dependency's rows are what this component's re-exports resolve
      -- against, so its version -- and, for a local one, its bytes --
      -- belong in here too.
    , [ unPackageName (pkgName d) <> "-" <> unVersion (pkgVersion d)
      | d <- deps ]
    , depFps
    ]
  where
    deps = sortOn (unPackageName . pkgName) $ case lookupUnit (pkgName pid) plan of
      Nothing -> []
      Just u  -> puDeps u

-- | The source-tree digest of a unit the plan gives a directory for, and a
-- constant for one it does not.  Never resolves a package, so calling it
-- cannot trigger an extraction.
mutableSourceFingerprint :: BuildPlan -> PackageName -> IO Text
mutableSourceFingerprint plan name =
  case puSrcDir =<< lookupUnit name plan of
    Just d  -> Fingerprint.componentFingerprint [d]
    Nothing -> pure "immutable"

-- | The language settings, flattened for hashing.
--
-- 'show' on GHC's 'Extension' is a serialisation here, not a rendering:
-- nothing displays this, and the compiler whose enum it is appears in the
-- same digest.
languageFingerprint :: LanguageSettings -> [Text]
languageFingerprint ls =
  Text.pack (show (Extensions.lsLanguage ls))
    : sort (map (Text.pack . show) (Extensions.lsDefaultOn ls))
    ++ ["/off"]
    ++ sort (map (Text.pack . show) (Extensions.lsDefaultOff ls))

-- | The packages a unit may resolve a re-export through: its dependencies,
-- plus its own name.
--
-- Its own name is in there because a sub-library re-exporting from the
-- package's main library crosses a /component/ boundary without crossing a
-- package one, and 'lookupExport' filters on package names.
dependencySet :: BuildPlan -> PackageId -> Set PackageName
dependencySet plan pid =
  Set.insert (pkgName pid) $ case lookupUnit (pkgName pid) plan of
    Nothing -> Set.empty
    Just u  -> Set.fromList (map pkgName (puDeps u))

-- | Pick the source roots to scan for a package.  If any of the common
-- @hs-source-dirs@ subdirectories exist we walk those exclusively;
-- otherwise we fall back to the package root.  Walking both root /and/
-- the @src/@ subtree double-counts modules and produces duplicate
-- "src.Foo.Bar" / "Foo.Bar" rows in the search index.
chooseSourceRoots :: FilePath -> IO [FilePath]
chooseSourceRoots d = do
  let candidates = [ d FP.</> sub
                   | sub <- ["src", "library", "lib", "Library", "source", "Source"] ]
  existingSubs <- filterM Dir.doesDirectoryExist candidates
  pure (if null existingSubs then [d] else existingSubs)

findHs :: FilePath -> Int -> IO [FilePath]
findHs _ depth | depth < 0 = pure []
findHs dir depth = do
  entries <- Dir.listDirectory dir
  let absEntries = map (dir FP.</>) entries
  concat <$> mapM (visit depth) absEntries
  where
    visit d p = do
      isDir <- Dir.doesDirectoryExist p
      if isDir
        then if skipDir (FP.takeFileName p)
               then pure []
               else findHs p (d - 1)
        else if ".hs" `Text.isSuffixOf` Text.pack p
               then pure [p]
               else pure []

    skipDir name = case name of
      '.':_ -> True
      "dist" -> True
      "dist-newstyle" -> True
      "test" -> True
      "tests" -> True
      "bench" -> True
      "benchmarks" -> True
      "Setup" -> True
      _ -> False

-- | A source-relative path to the module name it would hold.
--
-- Through 'System.FilePath' rather than by rewriting @\'\/\'@ to
-- @\'.\'@: the hand-rolled version knew only the POSIX separator, which
-- is the shape of the Windows bug in issue #10.
hsToModule :: FilePath -> String
hsToModule =
  intercalate "." . FP.splitDirectories . FP.dropExtension

-- The pure core ------------------------------------------------------

-- | An export the component does not itself declare.
--
-- Named rather than tupled because all three fields are module paths or
-- close to it, and a bare triple at a report site is unreadable.
data OutsideExport = OutsideExport
  { oeModule     :: !ModulePath     -- ^ the module that exports it
  , oeName       :: !SymbolName
  , oeCandidates :: ![ModulePath]
    -- ^ Every import that could supply it, ranked, all of them tried.
    -- A list rather than the single import we "believe" supplies it:
    -- an open @import Prelude@ plausibly supplies any name, so one
    -- candidate is a guess and @base@'s @Control.Concurrent@ is where
    -- that guess is always wrong.  Empty when no import could have
    -- supplied it at all — a class method has none.
  , oeVisibility :: !Visibility
    -- ^ The exporting module's visibility, carried so a repaired row can
    -- be built from this record alone.  Recomputing it later would mean
    -- re-deriving it from a module list the repair pass does not have.
  }
  deriving stock (Show, Eq, Ord)

-- | What indexing one component produced, and what it could not.
data ComponentIndex = ComponentIndex
  { ciRows          :: ![IndexRow]
  , ciParseFailures :: ![(ModulePath, Parser.ParseError)]
  , ciNameMismatch  :: ![(ModulePath, ModulePath)]
    -- ^ @(name the stanza expected, name the source declares)@.  Real in
    -- the wild, and silently trusting either side produces rows nobody
    -- can reach.
  , ciUnresolved    :: ![OutsideExport]
    -- ^ Exports whose definition lives outside the component and which no
    -- indexed dependency could supply.  A symbol missing from the index is
    -- invisible, and the user has no other way to find out.
  , ciAmbiguous     :: ![(OutsideExport, ExportChoice)]
    -- ^ Exports more than one dependency could have supplied.  Resolved,
    -- deterministically, and worth saying so.
  , ciExternalModuleForms :: ![(ModulePath, ModulePath)]
    -- ^ @(re-exporting module, the @module M@ it names)@ for an @M@ this
    -- component does not have.  Those names cannot be expanded, so they
    -- never reach 'ciUnresolved' either; without this field the loss is
    -- total and silent.
  , ciOriginFailures :: ![(ModulePath, Origins.OriginError)]
    -- ^ Modules whose compiled interface the repair pass could not read.
    -- Empty until 'repairUnresolved' has run.  A package that was never
    -- built has no interface files, which is ordinary and still worth
    -- saying: it is the difference between "nothing to repair" and "we
    -- never looked".
  }
  deriving stock (Show, Eq)

-- | Build a component's rows.
--
-- Pure, because every judgement in here — what a module is called, which
-- module defines a symbol, which signature it carries — is a function of
-- the sources.  Mixing those judgements with file IO is what let the old
-- indexer paper over a parse failure: it logged the failure, then let the
-- re-export pass invent rows for the module anyway, resolving their
-- signatures through a name-keyed map.
indexComponentPure
  :: ComponentKey
  -> Set PackageName          -- ^ the packages a re-export may resolve through
  -> ExportEnv                -- ^ what the components indexed so far export
  -> LanguageSettings
  -> [ModuleSource]
  -> ComponentIndex
indexComponentPure compKey deps env langs sources =
  indexParsedComponent compKey deps env
    [ (ms, Interface.parseInterface langs (msPath ms) (msContent ms))
    | ms <- sources
    ]

-- | The core, over parse results the caller obtained.
--
-- Parsing is the caller's job because it is where the exceptions are: see
-- 'Interface.parseInterfaceIO'.  Everything from here on is a function of
-- the sources and of what the dependencies exported.
indexParsedComponent
  :: ComponentKey
  -> Set PackageName
  -> ExportEnv
  -> [(ModuleSource, Either Parser.ParseError ModuleInterface)]
  -> ComponentIndex
indexParsedComponent compKey deps env parsed = ComponentIndex
  { ciRows          = localRows ++ outsideRows
  , ciParseFailures = failures
  , ciNameMismatch  = mismatches
  , ciUnresolved    = unresolved
  , ciAmbiguous     = ambiguous
  , ciExternalModuleForms = Reexport.externalModuleForms ifaces
  , ciOriginFailures = originFailures
  }
  where
    failures   = [ (msDeclaredName ms, e) | (ms, Left e)  <- parsed ]
    ok         = [ (ms, i)                | (ms, Right i) <- parsed ]
    mismatches =
      [ (msDeclaredName ms, miName i)
      | (ms, i) <- ok
      , msDeclaredName ms /= miName i
      ]

    ifaces = map snd ok

    -- Keyed on the name the source declares, which is the same key the
    -- resolution map uses.  One map rather than three with identical key
    -- sets: the two 'findWithDefault' defaults the split version needed
    -- could never fire, so a reader had to reconstruct that argument to
    -- know the empty signature was not a sentinel.
    byModule :: Map.Map ModulePath (ModuleSource, ModuleInterface)
    byModule = Map.fromList [ (miName i, (ms, i)) | (ms, i) <- ok ]

    -- Numbered once per module, not once per symbol.  Every row resolving
    -- into @Data.Map.Internal@ used to re-number its ~4800 lines to slice
    -- one signature out; the lazy map value makes it one traversal per
    -- module that any row actually lands in.
    linesOf :: Map.Map ModulePath [(Int, Text)]
    linesOf = Map.map (Parser.numberedLines . msContent . fst) byModule

    resolved = Map.toList (Reexport.resolveComponent ifaces)

    visibilityFor presented =
      maybe Internal (msVisibility . fst) (Map.lookup presented byModule)

    -- Exports this component declares, here or in a sibling module.
    --
    -- 'findDecl' cannot miss here: a site is only "inside" when
    -- 'Interface.declaredNames' found the name, and that reads the very
    -- 'miDecls' list 'findDecl' searches.  Class methods and data
    -- constructors are declarations in their own right now (they were
    -- the names this filter dropped before the parser learned to emit
    -- them), so a method resolves here and gets a row like any other
    -- declaration.
    localRows =
      [ IndexRow
          { rowComponent  = compKey
          , rowModule     = presented
          , rowName       = name
            -- The signature is read from the module the resolver landed
            -- on.  Looking it up in a component-wide name map is what
            -- published Data.IntMap.Lazy.insertWith with Data.Map's
            -- signature.  Empty only for the kinds that genuinely have no
            -- signature line — a type, class or plain binding without one
            -- — never for a constructor, which has its declaration.
          , rowSignature  = Signature
              (fromMaybe "" (Parser.declSigOrSliceIn (linesFor defMod) decl))
          , rowDefinition = DefinitionRef compKey defMod
          , rowVisibility = visibilityFor presented
          }
      | ((presented, name), site) <- resolved
      , defMod <- insideSite presented site
      , Just (_, defIface) <- [Map.lookup defMod byModule]
      , Just decl <- [Parser.findDecl (unSymbolName name) (miDecls defIface)]
      ]

    linesFor m = Map.findWithDefault [] m linesOf

    -- Exports whose definition is in a dependency, with every import that
    -- could have supplied them.
    outside =
      [ OutsideExport presented name cands (visibilityFor presented)
      | ((presented, name), site) <- resolved
      , Just cands <- [outsideCandidates site]
      ]

    -- The first candidate an indexed dependency actually exports wins.
    -- Probing rather than trusting the best-ranked one is the whole
    -- point: ranking can only order syntax, and syntax cannot tell an
    -- open import that supplies the name from one that does not.
    classified = [ (oe, firstResolvable oe) | oe <- outside ]

    firstResolvable oe = listToMaybe
      [ ch
      | m       <- oeCandidates oe
      , Just ch <- [lookupExport deps m (oeName oe) env]
      ]

    outsideRows = [ outsideRow compKey oe ch | (oe, Just ch) <- classified ]

    unresolved = [ oe | (oe, Nothing) <- classified ]

    originFailures = []

    ambiguous =
      [ (oe, ch)
      | (oe, Just ch) <- classified
      , not (null (ecRejected ch))
      ]

-- | The row an 'OutsideExport' becomes once something has told us where
-- its definition is.
--
-- Shared by the syntactic pass and the repair pass because they build the
-- same row from the same two things: they differ only in who answered.
-- 'oeVisibility' rather than a second lookup in the component's module
-- map — the repair pass does not have that map, and the export knows.
outsideRow :: ComponentKey -> OutsideExport -> ExportChoice -> IndexRow
outsideRow compKey oe ch = IndexRow
  { rowComponent  = compKey
  , rowModule     = oeModule oe
  , rowName       = oeName oe
  , rowSignature  = exSignature (ecChosen ch)
  , rowDefinition = exDefinition (ecChosen ch)
  , rowVisibility = oeVisibility oe
  }

-- | Resolve what syntax could not, by asking the compiler.
--
-- 'indexParsedComponent' can only rank a module's imports; it cannot know
-- which one supplies a name, because an open import supplies every name
-- syntactically.  GHC ran the renamer and wrote the answer into the
-- module's @.hi@ file, so for each export the pure pass left unresolved
-- we ask for the origin and re-run the same 'lookupExport' against it.
--
-- The signature still comes from the dependency's own indexed row, never
-- from the interface: a @.hi@ has no source text, and a row with an
-- invented signature is what "Hypha.Search.Reexport" exists to prevent.
-- An origin no indexed dependency exports therefore yields no row and the
-- export stays in the report — @Data.Bits.(.&.)@ is a class method, so
-- @ghc-internal@ has no row for it either (issue 043).
--
-- One interface read per module that has unresolved exports, not per
-- component and not per export: a warm component asks nothing at all.
repairUnresolved
  :: Monad m
  => Origins.OriginOracle m
  -> ComponentKey
  -> PackageId                -- ^ the package whose interfaces to read
  -> Set PackageName          -- ^ the packages a re-export may resolve through
  -> ExportEnv
  -> ComponentIndex
  -> m ComponentIndex
repairUnresolved oracle compKey pid deps env ci = do
  attempted <- mapM askAbout (Map.toList grouped)
  let failures = [ (m, e)  | (m, Left e)   <- attempted ]
      answered = concat [ rs | (_, Right rs) <- attempted ]
      repaired = Set.fromList (map fst answered)
  pure ci
    { ciRows           = ciRows ci ++ map (uncurry rowFor) answered
    , ciUnresolved     = [ oe | oe <- ciUnresolved ci
                              , not (oe `Set.member` repaired) ]
    , ciAmbiguous      = ciAmbiguous ci
                           ++ [ (oe, ch)
                              | (oe, ch) <- answered
                              , not (null (ecRejected ch)) ]
    , ciOriginFailures = ciOriginFailures ci ++ failures
    }
  where
    grouped = Map.fromListWith (++)
      [ (oeModule oe, [oe]) | oe <- ciUnresolved ci ]

    askAbout (m, oes) = do
      answer <- Origins.moduleOrigins oracle pid m
      pure (m, fmap (resolveAgainst oes) answer)

    -- One name can have more than one origin -- ghc's own @GHC@ exports
    -- @XFixitySig@ from two modules -- so these are probed in order too,
    -- for the same reason the candidate imports are.
    resolveAgainst oes mo =
      [ (oe, ch)
      | oe           <- oes
      , Just origins <- [Map.lookup (oeName oe) (Origins.moOrigins mo)]
      , Just ch      <- [firstFrom (oeName oe) (NE.toList origins)]
      ]

    firstFrom name origins = listToMaybe
      [ ch | o <- origins, Just ch <- [lookupExport deps o name env] ]

    rowFor = outsideRow compKey

-- | The definition module when it is inside this component, and nothing
-- when it is not.  A list rather than a 'Maybe' so it drops straight into
-- the row comprehension.
insideSite :: ModulePath -> DefinitionSite -> [ModulePath]
insideSite asking site = case site of
  DefinedHere      -> [asking]
  DefinedIn m      -> [m]
  DefinedOutside _ -> []
  NoSupplier       -> []

-- | The imports a site offers as suppliers, and nothing when the site is
-- inside the component.  'NoSupplier' offers an empty list rather than
-- nothing: the export is real, we simply have no candidate to try, and
-- it still owes the reader an entry in the unresolved report.
outsideCandidates :: DefinitionSite -> Maybe [ModulePath]
outsideCandidates site = case site of
  DefinedHere       -> Nothing
  DefinedIn _       -> Nothing
  DefinedOutside ms -> Just (NE.toList ms)
  NoSupplier        -> Just []

-- | Trace what a component's index pass could not do.  Never silent: a
-- module or symbol missing from the index is invisible to search, and the
-- user has no other way to find out.
reportComponentIndex :: ComponentKey -> ComponentIndex -> IO ()
reportComponentIndex compKey ci = do
  mapM_ reportFailure    (ciParseFailures ci)
  mapM_ reportMismatch   (ciNameMismatch ci)
  mapM_ reportUnresolved (ciUnresolved ci)
  mapM_ reportAmbiguous  (ciAmbiguous ci)
  mapM_ reportExternalForm (ciExternalModuleForms ci)
  mapM_ reportOriginFailure (ciOriginFailures ci)
  where
    label = Text.unpack (unComponentKey compKey)

    reportOriginFailure (m, e) = hPutStrLn stderr $
      "hypha index: " <> label <> " could not read the compiled interface for "
        <> Text.unpack (unModulePath m) <> ": "
        <> Text.unpack (Origins.renderOriginError e)

    reportExternalForm (from, m) = hPutStrLn stderr $
      "hypha index: " <> label <> " re-exports module "
        <> Text.unpack (unModulePath m) <> " from "
        <> Text.unpack (unModulePath from)
        <> ", which is not part of this component; its names are not indexed"

    -- Naming every candidate, not just the best one: "could not resolve
    -- it through Prelude" sent every reader after the wrong import.
    reportUnresolved oe = hPutStrLn stderr $
      "hypha index: " <> label <> " could not resolve "
        <> Text.unpack (unModulePath (oeModule oe)) <> "."
        <> Text.unpack (unSymbolName (oeName oe))
        <> case oeCandidates oe of
             [] -> "; no import of that module could supply it"
             ms -> " through any of "
                     <> intercalate ", "
                          (map (Text.unpack . unModulePath) ms)
                     <> "; no indexed dependency exports it"

    reportAmbiguous (oe, ch) = hPutStrLn stderr $
      "hypha index: " <> label <> " resolved "
        <> Text.unpack (unModulePath (oeModule oe)) <> "."
        <> Text.unpack (unSymbolName (oeName oe)) <> " to "
        <> renderRef (exDefinition (ecChosen ch)) <> ", rejecting "
        <> unwords (map renderRef (ecRejected ch))

    renderRef r =
      Text.unpack (unComponentKey (drComponent r)) <> ":"
        <> Text.unpack (unModulePath (drModule r))

    reportFailure (m, e) = hPutStrLn stderr $
      "hypha index: " <> label <> " skipped module "
        <> Text.unpack (unModulePath m) <> ": "
        <> Text.unpack (Parser.parseErrorMessage e)

    reportMismatch (declared, actual) = hPutStrLn stderr $
      "hypha index: " <> label <> " expected module "
        <> Text.unpack (unModulePath declared) <> " but its source declares "
        <> Text.unpack (unModulePath actual) <> "; using the latter"

-- | The modules of one component, as the cabal stanza lists them.
--
-- Enumeration from @exposed-modules@ + @other-modules@ is what keeps a
-- stray script under a source dir from becoming a module: the filesystem
-- walk that used to do this turned @examples/race.hs@ into a module called
-- @race@.  The walk survives only for components whose cabal we could not
-- parse, and says so when it fires.
componentModules
  :: BuildPlan
  -> PackageId
  -> Comp.ComponentKind
  -> [FilePath]
  -> IO [ModuleSource]
componentModules plan pid kind srcDirs =
  case componentInfoFor plan (pkgName pid) kind of
    Just ci
      | not (null (stanzaModules ci)) -> load (stanzaModules ci)
    _ -> do
      hPutStrLn stderr $
        "hypha index: " <> Text.unpack (unPackageName (pkgName pid))
          <> " has no cabal module list; falling back to a source-dir walk"
      walked <- enumModulesIn srcDirs
      load [ (m, Exposed) | m <- walked ]
  where
    load = loadModuleSources srcDirs

-- | Read each named module from the first source dir that has it.
--
-- Modules the stanza names but whose file we cannot find are dropped: a
-- @.hsc@ or @.chs@ source we do not preprocess is a real case, and it is
-- the module's rows we lose, not the component's.
loadModuleSources :: [FilePath] -> [(Text, Visibility)] -> IO [ModuleSource]
loadModuleSources srcDirs = fmap catMaybes . mapM loadOne
  where
    loadOne (modPath, vis) = do
      mFile <- firstExistingModule srcDirs modPath
      case mFile of
        Nothing -> pure Nothing
        Just f  -> do
          content <- readSourceFile f
          pure (Just ModuleSource
            { msDeclaredName = ModulePath modPath
            , msPath         = f
            , msVisibility   = vis
            , msContent      = content
            })

    firstExistingModule [] _ = pure Nothing
    firstExistingModule (r : rs) modPath = do
      let candidate =
            r FP.</> FP.joinPath (map Text.unpack (Text.splitOn "." modPath))
              FP.<.> "hs"
      ok <- Dir.doesFileExist candidate
      if ok then pure (Just candidate) else firstExistingModule rs modPath

-- | The modules a cabal stanza names, with the visibility it gives them.
stanzaModules :: Comp.ComponentInfo -> [(Text, Visibility)]
stanzaModules ci =
  [ (m, Exposed)  | m <- Comp.ciExposedModules ci ]
    ++ [ (m, Internal) | m <- Comp.ciOtherModules ci ]

-- | Every library component of a package, read straight from its cabal
-- file without a build plan.
--
-- The CLI's @hypha source@ has a package directory and no plan, but still
-- needs the component's module list to resolve a re-export: without it the
-- only option is the package-wide sweep, which is a guess.
--
-- An empty result is always announced: 'Comp.parseLibComponents' returns
-- @[]@ both for a cabal file we cannot read and for one that names no
-- library, and a caller told only "no components" would silently fall
-- back to the sweep it was written to avoid.
packageSources
  :: FilePath
  -> BuildContext
     -- ^ So a module read on this path is preprocessed against the same
     -- macros and headers, and its stanzas resolved for the same
     -- platform, as the indexer used.  'hostBuildContext' outside a
     -- project, where there is no plan to derive one from.
  -> IO [(Comp.ComponentInfo, [ModuleSource])]
packageSources pkgRoot ctx = do
  mCabal <- Comp.findCabalFile pkgRoot
  case mCabal of
    Nothing    -> do
      report "has no cabal file"
      pure []
    Just cabal -> do
      comps <- Comp.parseLibComponents cabal pkgRoot ctx
      if null comps
        then do
          report ("cabal file " <> cabal <> " named no library component")
          pure []
        else mapM withSources comps
  where
    report why = hPutStrLn stderr $
      "hypha: " <> pkgRoot <> " " <> why
        <> "; a symbol can only be located by sweeping the package"

    withSources ci = do
      srcs <- loadModuleSources (Comp.ciHsSourceDirs ci) (stanzaModules ci)
      pure (ci, srcs)

-- | The parsed cabal component matching a kind, when we have one.
--
-- Keyed on the package /name/: 'lookupUnit' is, and a caller holding only
-- a component key (the server, from a URL) has no version to offer.
componentInfoFor
  :: BuildPlan -> PackageName -> Comp.ComponentKind -> Maybe Comp.ComponentInfo
componentInfoFor plan pkg kind = do
  pu <- lookupUnit pkg plan
  case [ c | c <- puLibComponents pu, Comp.ciKind c == kind ] of
    (c : _) -> Just c
    []      -> Nothing

-- | The language settings a component fixes for its modules.  Without them
-- a module relying on a stanza-wide extension parses differently for us
-- than for the compiler.
--
-- The server calls this too, on the way to a module page: two derivations
-- would mean the page could parse a module differently from the way the
-- index did, which is the divergence this layer exists to remove.
languageSettingsFor
  :: BuildPlan -> PackageName -> Comp.ComponentKind -> LanguageSettings
languageSettingsFor plan pkg kind =
  case componentInfoFor plan pkg kind of
    Just ci -> Comp.ciLanguageSettings ci
    Nothing -> Extensions.defaultLanguageSettings

-- | Everything one /component's/ rows contribute to the in-memory scorer:
-- one row per symbol, plus the module rows they imply.
--
-- Both the build path and the hydrate-from-cache path go through here, so
-- a warm cache and a fresh index cannot disagree about which entities are
-- searchable.  The package row is published once per unit by the caller,
-- since a package is not a per-component fact.
componentScorerRows :: [IndexRow] -> [Fuzzy.IndexedRow]
componentScorerRows rows =
  Fuzzy.moduleRows rows ++ map Fuzzy.mkSymbolRow rows

-- | Publish rows into the scorer's shared view.
--
-- The spine is forced before the critical section, not after: the rows are
-- built per module and 'seq' on the list only forced its first cell, so the
-- lowercasing every 'Fuzzy.indexedRow' does was still a thunk the /search/
-- thread paid for on the first query.
publishRows :: IORef.IORef [Fuzzy.IndexedRow] -> [Fuzzy.IndexedRow] -> IO ()
publishRows ref rows = do
  n <- Exception.evaluate (length rows)
  n `seq` IORef.atomicModifyIORef' ref (\old -> (rows ++ old, ()))

