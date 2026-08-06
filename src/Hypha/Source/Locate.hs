{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Source.Locate
  ( listExportedSymbols
  , locateSymbolDefinition
  , locateSymbolDefinitionInDir
  , locateDefinitionInComponent
  , locateDefinitionInComponentWith
  , SearchBounds (..)
  , defaultSearchBounds
  , LocatedDefinition (..)
  , findModuleFile
  , findModuleFileIn
  , SourceLocation (..)
    -- * Testing
  , exportedNamesOf
  , modulePathToFile
  ) where

import Control.Monad (filterM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Control.Monad.Trans.State.Strict
  (evalStateT, get, gets, modify', put)
import Data.List (sortOn)
import Data.List.NonEmpty qualified as NE
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>))

import Hypha.BuildEnv.Type     (BuildEnv (..))
import Hypha.Search.Index      (DefinitionRef (..), ModuleSource (..))
import Hypha.Search.Reexport   (DefinitionSite (..))
import Hypha.Source.Reach
  ( OutsideModule (..), OutsideReach (..), SymbolSearchFailure (..) )
import qualified Hypha.Search.Reexport as Reexport
import qualified Hypha.Source.Extensions as Extensions
import qualified Hypha.Source.Interface as Interface
import qualified Hypha.Source.Parser as Parser
import Hypha.Types.ComponentName (ComponentKey (..))
import Hypha.Types.PackageId   (PackageId (..))
import Hypha.Types.SymbolPath  (ModulePath (..), SymbolName (..))
import System.IO (hPutStrLn, stderr)

-- | Location of a symbol definition in a source file.
data SourceLocation = SourceLocation
  { slPath :: !FilePath
  , slLine :: !Int
  }
  deriving stock (Show, Eq)

-- | The symbols a module of a package exports, read from its parse tree.
--
-- Its predecessor scraped the module header with a hand-rolled scanner
-- that could not tell @Map(..)@ from @Map@ and dropped the @module N@
-- re-export form -- the form @containers@' public modules are largely
-- built from.  "Hypha.Source.Interface" replaced it; this call site
-- outlived the replacement.
listExportedSymbols :: BuildEnv IO -> PackageId -> Text -> IO [Text]
listExportedSymbols env pid modPath = do
  mDir <- locatePackageSource env pid
  case mDir of
    Nothing -> pure []
    Just d  -> do
      let f = d </> modulePathToFile modPath
      ok <- doesFileExist f
      if not ok
        then pure []
        else exportedNamesOf Extensions.defaultLanguageSettings f
               =<< TIO.readFile f

-- | 'Interface.exportedNamesIO' with the failure reported rather than
-- rendered as "this module exports nothing", which is what an empty list
-- looks like to every caller.
exportedNamesOf :: Extensions.LanguageSettings -> FilePath -> Text -> IO [Text]
exportedNamesOf ls f src = do
  r <- Interface.exportedNamesIO ls f src
  case r of
    Right ns -> pure (map unSymbolName ns)
    Left e   -> do
      hPutStrLn stderr $
        "hypha: " <> f <> " could not be parsed: "
          <> Text.unpack (Parser.parseErrorMessage e)
          <> "; its export list is unavailable"
      pure []

-- | Convert a module path like @Control.Concurrent.Async@ to a file path
-- like @Control/Concurrent/Async.hs@.
modulePathToFile :: Text -> FilePath
modulePathToFile m = Text.unpack (Text.replace "." "/" m) <> ".hs"

-- | Locate the source file for a module under a package's source tree.
-- Tries the root first, then common @hs-source-dirs@ ("src", "library",
-- "lib", "Library"), then a bounded BFS for any prefix dir that contains
-- the module path.
findModuleFile :: FilePath -> Text -> IO (Maybe FilePath)
findModuleFile root modPath = do
  let relFile  = modulePathToFile modPath
      candidates =
        [ root </> relFile
        , root </> "src"     </> relFile
        , root </> "library" </> relFile
        , root </> "lib"     </> relFile
        , root </> "Library" </> relFile
        , root </> "source"  </> relFile
        , root </> "Source"  </> relFile
        ]
  m <- firstExisting candidates
  case m of
    Just p  -> pure (Just p)
    Nothing -> bfsFind root relFile 4

-- | Resolve a module path against an explicit list of source-root
-- candidates, in priority order.  The first existing file wins.
-- Unlike 'findModuleFile' this does /not/ walk heuristic subdirs — the
-- caller is expected to have already enumerated the component's
-- @hs-source-dirs@.
findModuleFileIn :: [FilePath] -> Text -> IO (Maybe FilePath)
findModuleFileIn roots modPath =
  let rel = modulePathToFile modPath
  in firstExisting [ r </> rel | r <- roots ]

firstExisting :: [FilePath] -> IO (Maybe FilePath)
firstExisting []     = pure Nothing
firstExisting (p:ps) = do
  ok <- doesFileExist p
  if ok then pure (Just p) else firstExisting ps

-- | Bounded breadth-first search.  Visits @root@'s subdirectories up to the
-- given depth looking for any @<subdir>/<rel>@ file.  Skips hidden and
-- build-artifact directories.
bfsFind :: FilePath -> FilePath -> Int -> IO (Maybe FilePath)
bfsFind root rel depth
  | depth <= 0 = pure Nothing
  | otherwise = do
      entries <- listDirectory root
      let dirs = [ root </> e | e <- entries, not (isSkip e) ]
      subs <- filterM doesDirectoryExist dirs
      directHit <- firstExisting [ d </> rel | d <- subs ]
      case directHit of
        Just p  -> pure (Just p)
        Nothing -> tryDeeper subs
  where
    tryDeeper []     = pure Nothing
    tryDeeper (d:ds) = do
      m <- bfsFind d rel (depth - 1)
      case m of
        Just p  -> pure (Just p)
        Nothing -> tryDeeper ds

    isSkip name = case name of
      '.':_       -> True
      "dist"      -> True
      "dist-newstyle" -> True
      "build"     -> True
      "test"      -> True
      "tests"     -> True
      "bench"     -> True
      "benchmarks" -> True
      _           -> False

-- | Find the line where a symbol is defined.
locateSymbolDefinition :: BuildEnv IO -> PackageId -> Text -> Text -> IO (Maybe SourceLocation)
locateSymbolDefinition env pid modPath sym = do
  mDir <- locatePackageSource env pid
  case mDir of
    Nothing -> pure Nothing
    Just d  -> locateSymbolDefinitionInDir d modPath sym

-- | Variant that takes a pre-resolved source directory (e.g. from
-- 'Hypha.Package.Resolver.resolveSrc').  Uses 'findModuleFile' so common
-- @hs-source-dirs@ layouts are covered.
locateSymbolDefinitionInDir :: FilePath -> Text -> Text -> IO (Maybe SourceLocation)
locateSymbolDefinitionInDir d modPath sym = do
  mFile <- findModuleFile d modPath
  case mFile of
    Nothing -> findInTree d modPath sym
    Just f  -> do
      scanned <- scanFileE sym f
      case scanned of
        Left e -> do
          -- Not \"not here\": we could not read the module at all.  Say so,
          -- then sweep, so a wrong answer is at least an explained one.
          hPutStrLn stderr $
            "hypha: " <> f <> " could not be parsed: "
              <> Text.unpack (Parser.parseErrorMessage e)
          findInTree d modPath sym
        Right (Just loc) -> pure (Just loc)
        Right Nothing    -> findInTree d modPath sym

-- | Where a symbol is declared.
--
-- There is no confidence field: 'locateDefinitionInComponent' either
-- resolves or says why it could not, so a @GuessedBySweep@ arm was a state
-- the code could not construct -- and the "Best guess" warning it drove
-- was UI no reader could ever see.
data LocatedDefinition = LocatedDefinition
  { ldLocation   :: !SourceLocation
  , ldModule     :: !ModulePath
  , ldComponent  :: !ComponentKey
    -- ^ Which component the definition is in.  A re-export can cross a
    -- package boundary, and a card reporting only the module would send the
    -- reader to a module the page's package does not have.
  , ldDecl       :: !Parser.Decl
    -- ^ The declaration itself, so a caller building a symbol card reads
    -- the signature and Haddock off the parse that located it rather than
    -- reading and parsing the file a second time — which is what the card
    -- used to do, under the wrong language settings and discarding the
    -- parse error.
  , ldContent    :: !Text
    -- ^ The defining module's bytes, for slicing the declaration's text.
  }
  deriving stock (Show, Eq)

-- | Locate a symbol given every module of its component, plus whatever of
-- other components the caller can reach.
--
-- Resolution, not sweeping: with the component in hand there is nothing to
-- guess.  A symbol the asking module re-exports is followed to its
-- definition — across package boundaries, and across as many of them as
-- the chain crosses — and a symbol whose definition we cannot reach is
-- reported absent rather than approximated by the first same-named binding
-- elsewhere in the package.
--
-- The component key is a parameter because the result has to name the
-- component the definition is in, and deriving that from a module path is
-- exactly the guess this module exists to remove.
locateDefinitionInComponent
  :: Extensions.LanguageSettings
  -> ComponentKey                                  -- ^ the asking component
  -> [ModuleSource]
  -> OutsideReach IO                               -- ^ what lies outside it
  -> ModulePath
  -> SymbolName
  -> IO (Either SymbolSearchFailure LocatedDefinition)
locateDefinitionInComponent = locateDefinitionInComponentWith defaultSearchBounds

-- | 'locateDefinitionInComponent' with the search bounds supplied.
--
-- A parameter rather than a constant so a test can trip a limit without
-- authoring a sixty-five module fixture: the distinction between "stopped
-- early" and "not there" is the reason those limits are typed, and an
-- untestable distinction is one nobody will keep working.
locateDefinitionInComponentWith
  :: SearchBounds
  -> Extensions.LanguageSettings
  -> ComponentKey                                  -- ^ the asking component
  -> [ModuleSource]
  -> OutsideReach IO                               -- ^ what lies outside it
  -> ModulePath
  -> SymbolName
  -> IO (Either SymbolSearchFailure LocatedDefinition)
locateDefinitionInComponentWith bounds langs ownComponent sources reach asking sym =
  -- The index's answer first when there is one: it was resolved once, for
  -- the whole component, and taking it costs a single parse where
  -- following the chain costs one per hop.  Resolving from source is the
  -- fallback, not the lesser answer — it reaches the same place, and it is
  -- the only path the CLI has.
  resolvedSite >>= \case
    Just (def, om) -> do
      found <- scanned (drComponent def) (drModule def) (omSource om)
                 (omLanguage om) Nothing
      -- A shortcut that does not pan out is still only a shortcut: an
      -- index row can name a site whose module no longer declares the
      -- name.  Falling through to the chain costs a component resolution
      -- and answers; treating it as absence costs nothing and lies.
      maybe byResolution (pure . Right) found
    Nothing -> byResolution
  where
    resolvedSite = runMaybeT $ do
      def <- MaybeT (pure (Map.lookup sym (orSites reach)))
      om  <- MaybeT (orModule reach (drModule def))
      pure (def, om)

    byResolution = do
      parsed <- Interface.parseSources langs sources
      mapM_ reportParseFailure [ (ms, e) | (ms, Left e) <- parsed ]
      let ifaces     = [ i | (_, Right i) <- parsed ]
          resolution = Reexport.resolveComponent ifaces
      -- The caller only asks about a module its component lists, so a
      -- module missing from the parses is one that would not parse.  Saying
      -- "does not export" there — which is what falling straight through to
      -- the resolution lookup did — reports a module we could not read as a
      -- module we read and found wanting.
      if asking `notElem` map Interface.miName ifaces
        then pure (Left (SearchModuleUnparsed asking))
        else case Map.lookup (asking, sym) resolution of
          Nothing       -> pure (Left SearchNotExported)
          Just resolved -> case resolved of
            -- Every candidate import, in rank order: the best-ranked one is
            -- syntax, not an answer, and @base@'s @Control.Concurrent@
            -- ranks @Prelude@ ahead of the module that really declares
            -- @isCurrentThreadBound@.
            DefinedOutside ms -> followChain (localSources parsed) (NE.toList ms)
            site -> do
              let target = Reexport.definitionModule asking site
              case [ (ms, i)
                   | (ms, Right i) <- parsed, Interface.miName i == target ] of
                []            -> pure (Left (SearchModuleUnparsed target))
                ((ms, i) : _) -> do
                  found <- scanned ownComponent target ms langs (Just i)
                  pure (maybe (Left (SearchNotDeclared target)) Right found)

    -- The parse we already have, or one made under the settings of the
    -- stanza the module came from -- never 'scanFileE', which re-reads the
    -- file and parses it under the GHC2021 floor.  That discarded the
    -- cabal stanza's default-extensions at the last hop, so a component
    -- with @default-extensions: LambdaCase@ resolved the symbol and then
    -- failed to read the module it had resolved it to.
    scanned comp target ms ls mIface = do
      mIface' <- maybe (parsedOf ls ms) (pure . Just) mIface
      pure (declaredIn comp target ms =<< mIface')

    parsedOf ls ms = do
      r <- Interface.parseInterfaceIO ls (msPath ms) (msContent ms)
      case r of
        Left e      -> reportParseFailure (ms, e) >> pure Nothing
        Right iface -> pure (Just iface)

    declaredIn comp target ms iface = do
      decl <- Parser.findDecl (unSymbolName sym) (Interface.miDecls iface)
      ln   <- declAnchor decl
      pure LocatedDefinition
        { ldLocation   = SourceLocation (msPath ms) ln
        , ldModule     = target
        , ldComponent  = comp
        , ldDecl       = decl
        , ldContent    = msContent ms
        }

    -- The component's own modules, so a candidate that lives here is read
    -- from the bytes we already hold rather than looked for outside.
    -- Keyed on the parsed name, which is what an import names.
    localSources parsed = Map.fromList
      [ (Interface.miName i, (ownComponent, ms)) | (ms, Right i) <- parsed ]

    -- Follow the re-export chain hop by hop.
    --
    -- One ring of candidates is not enough.  The first hop out of
    -- @base:Data.List@ lands in @GHC.Internal.Data.List@, which is itself
    -- a facade and passes @sortOn@ on to @GHC.Internal.Data.OldList@ —
    -- stopping at the ring reports the symbol absent one module short of
    -- it, which is issue #20.  Each hop asks the module it just read which
    -- of /its/ imports could supply the name, using the same ranking the
    -- component-wide resolution uses.
    --
    -- Breadth-first, so a nearer definition always wins over a further
    -- one, and short-circuited within a level, so the common single
    -- candidate costs a single parse.  Bounded three ways, because an
    -- unrestricted import plausibly supplies any name and the candidate
    -- ranking is therefore an order over a graph rather than a path: a
    -- visited set, a hop limit, and a parse budget.  Which bound stopped
    -- the search is part of the answer, not a footnote on stderr.
    followChain local ring =
      evalStateT (hop (sbHopLimit bounds) ring) initialSearch
      where
        initialSearch = Search Set.empty (Remaining (sbParseBudget bounds))

        hop hops frontier = do
          st <- get
          -- Filtered first, so a chain that ended exactly at the limit
          -- reports having found nothing rather than claiming there was
          -- more to explore.
          case filter (`Set.notMember` seSeen st) frontier of
            []    -> pure (Left (SearchNoSupplier ring))
            fresh
              | hops <= 0 -> pure (Left (SearchHopLimit (sbHopLimit bounds) fresh))
              | otherwise -> level fresh >>= \case
                  Found ld               -> pure (Right ld)
                  Children explicit open -> do
                    spent <- gets ((== Exhausted) . seBudget)
                    if spent
                      then pure (Left (SearchParseBudget (sbParseBudget bounds)))
                      else hop (hops - 1) (explicit <> open)

        -- One ring of candidates: the first that declares the symbol wins,
        -- and the ones that do not contribute their own candidates to the
        -- next ring.  The two groups stay apart all the way across the
        -- level, so an explicitly-importing child of the /last/ parent
        -- still outranks an open-import child of the first.
        level = go [] []
          where
            go ex op [] = pure (children ex op)
            go ex op (m : ms) = probe m >>= \case
              ProbeDeclares ld    -> pure (Found ld)
              ProbeSupplies e o   -> go (e : ex) (o : op) ms
              ProbeUnreadable     -> go ex op ms
              -- Nothing further this level: the budget is gone, and 'hop'
              -- turns that into the report rather than another ring.
              ProbeExhausted      -> pure (children ex op)

            children ex op =
              Children (concat (reverse ex)) (concat (reverse op))

        probe m = do
          -- Marked seen whether or not it can be read: an unreachable
          -- candidate is unreachable every time it is offered.
          modify' (\st -> st { seSeen = Set.insert m (seSeen st) })
          lift (reachModule m) >>= \case
            Nothing -> pure ProbeUnreadable
            -- Charged here, not above: the budget counts parses, and a
            -- candidate no dependency could supply costs none.  Charging
            -- for lookups let a wide ring of unreachable names spend a
            -- budget that the modules past them needed.
            Just (comp, ms, ls) -> spend >>= \case
              False -> pure ProbeExhausted
              True  -> lift (parsedOf ls ms) >>= \case
                Nothing    -> pure ProbeUnreadable
                Just iface -> pure (verdict comp m ms iface)

        -- Declaring the name is not enough: a chain hop can only be
        -- supplied by a module that also /presents/ it, and a module
        -- reached through an open import may well have a private helper of
        -- the same name.  Taking that as the definition site is how a
        -- short name like @lines@ lands on the wrong file.
        verdict comp m ms iface =
          case declaredIn comp m ms iface of
            Just ld | exports iface -> ProbeDeclares ld
            _ -> uncurry ProbeSupplies (Reexport.supplierCandidatesByKind iface sym)

        spend = do
          st <- get
          case seBudget st of
            Exhausted   -> pure False
            Remaining 0 -> put st { seBudget = Exhausted } >> pure False
            Remaining n -> put st { seBudget = Remaining (n - 1) } >> pure True

        reachModule m = case Map.lookup m local of
          -- Our own component's modules are parsed under our own settings;
          -- a dependency's under the ones its stanza sets.
          Just (comp, ms) -> pure (Just (comp, ms, langs))
          Nothing         -> fmap outside <$> orModule reach m

        outside om = (omComponent om, omSource om, omLanguage om)

    -- | Whether a module's export list could be presenting this name.
    --
    -- @T(..)@ and @module M@ both count as "could": their subordinates are
    -- not resolvable from this module's parse alone, so reading them as
    -- "does not export" would reject hops that work today.  The check is
    -- therefore only decisive for a module whose export list is plain
    -- names — which is what the @GHC.Internal.*@ chain is made of, and
    -- where a private homonym is a real risk.
    exports iface = case Interface.miExports iface of
      Nothing    -> True
      Just items -> any covers items
      where
        covers item = case item of
          Interface.ExportSymbol n subs -> sym == n || sym `elem` subs
          Interface.ExportSymbolAll _   -> True
          Interface.ExportModule _      -> True

    reportParseFailure (ms, e) = hPutStrLn stderr $
      "hypha: " <> msPath ms <> " could not be parsed: "
        <> Text.unpack (Parser.parseErrorMessage e)

-- | What reading one candidate module told us.
data Probe
    -- | It declares the symbol, and presents it.
  = ProbeDeclares !LocatedDefinition
    -- | It does not, but these of its own imports could supply it:
    -- the ones that name it explicitly, then the open ones.
  | ProbeSupplies ![ModulePath] ![ModulePath]
    -- | Out of reach, or unreadable.  Nothing learned.
  | ProbeUnreadable
    -- | The parse budget ran out before this candidate could be read.
  | ProbeExhausted

-- | What walking one ring of candidates told us.
data Level
  = Found !LocatedDefinition
    -- | The next ring, with the explicitly-importing candidates of every
    -- parent ahead of the open-import ones.
  | Children ![ModulePath] ![ModulePath]

-- | What a chain-following search has spent so far.
--
-- Carried in a 'StateT' rather than threaded through the recursion,
-- because both fields are global to the search and a parameter version
-- would have to return them from every arm — including the arms that
-- answer, which is where they would get dropped and the visited set would
-- silently stop working.
data Search = Search
  { seSeen   :: !(Set ModulePath)
  , seBudget :: !Budget
  }

-- | Modules the search may still parse.
--
-- 'Exhausted' is a state of its own rather than a count of zero because it
-- also means \"and we have already told the user\": a counter alone would
-- either say nothing when the search gave up or repeat itself once per
-- module in a frontier hundreds wide.
data Budget = Remaining !Int | Exhausted
  deriving stock (Eq)

-- | How far a chain-following search may go before it reports that it
-- stopped.
data SearchBounds = SearchBounds
  { sbHopLimit    :: !Int
    -- ^ How many re-export hops to follow.
  , sbParseBudget :: !Int
    -- ^ How many modules to parse.
    --
    -- A hop limit alone does not bound the work: an unrestricted import is
    -- a candidate for every name, so the second ring out of a module like
    -- @base@'s @Control.Concurrent@ is already hundreds of modules wide.
    -- The searches that succeed cost a handful of parses; this only stops
    -- the ones that were never going to.
  }
  deriving stock (Show, Eq)

-- | Three hops, because two is what @base@ needs — @Data.List@ to
-- @GHC.Internal.Data.List@ to @GHC.Internal.Data.OldList@ — and a facade
-- over a facade over a facade is the deepest shape anyone has written on
-- purpose.
defaultSearchBounds :: SearchBounds
defaultSearchBounds = SearchBounds
  { sbHopLimit    = 3
  , sbParseBudget = 64
  }

-- | Walk every @.hs@ file under @root@ (skipping build/test dirs) and
-- return the first hit whose top-level binding or type signature matches
-- @sym@.  Used as a fallback when the target module re-exports a symbol
-- defined in another module of the same package (e.g.
-- @Data.Map.Strict.lookup@ re-exported from @Data.Map.Internal@).
findInTree :: FilePath -> Text -> Text -> IO (Maybe SourceLocation)
findInTree root modPath sym = do
  hsFiles <- enumerateHs root 6
  go (rankBySharedSuffix modPath hsFiles)
  where
    go []     = pure Nothing
    go (f:fs) = do
      scanned <- scanFileE sym f
      case scanned of
        Right (Just loc) -> pure (Just loc)
        _                -> go fs

-- | Rank candidate files by how much of the module's path they share,
-- counted in /segments/ from the end.
--
-- @Data.Map.Internal@ shares three trailing segments with
-- @…\/src\/Data\/Map\/Internal.hs@ and one with @…\/src\/Data\/Set\/Internal.hs@.
-- The predecessor counted shared leading /characters/ between a dotted
-- module prefix and a slashed path, which stopped discriminating after the
-- first segment and let the sweep answer @Data.Map.Internal.balanceL@ with
-- @Data\/Set\/Internal.hs@.
rankBySharedSuffix :: Text -> [FilePath] -> [FilePath]
rankBySharedSuffix modPath = sortOn (negate . shared)
  where
    wanted = reverse (Text.splitOn "." modPath)

    shared fp =
      let segs = reverse (Text.splitOn "/" (Text.pack (dropDotHs fp)))
      in length (takeWhile id (zipWith (==) wanted segs))

    dropDotHs fp = case Text.stripSuffix ".hs" (Text.pack fp) of
      Just t  -> Text.unpack t
      Nothing -> fp

-- | Bounded recursive enumeration of every @.hs@ file under @root@.
-- Skips hidden directories and conventional non-library trees so the
-- @findInTree@ fallback stays cheap on real packages.
enumerateHs :: FilePath -> Int -> IO [FilePath]
enumerateHs _ depth | depth < 0 = pure []
enumerateHs dir depth = do
  entries <- listDirectory dir
  fmap concat . mapM (visit depth) $ map (dir </>) entries
  where
    visit d p = do
      isDir <- doesDirectoryExist p
      if isDir
        then if skipDir (fileName p)
               then pure []
               else enumerateHs p (d - 1)
        else if ".hs" `Text.isSuffixOf` Text.pack p
               then pure [p]
               else pure []

    fileName :: FilePath -> String
    fileName = reverse . takeWhile (/= '/') . reverse

    skipDir n = case n of
      '.':_           -> True
      "dist"          -> True
      "dist-newstyle" -> True
      "build"         -> True
      "test"          -> True
      "tests"         -> True
      "bench"         -> True
      "benchmarks"    -> True
      _               -> False

-- | Find the line in a file where @sym@ has a top-level signature or
-- definition, using "Hypha.Source.Parser" so multi-symbol signatures
-- (@a, b :: T@), operator declarations, and other shapes the previous
-- line-grep missed all resolve correctly.  Definition line wins over
-- signature line when both are present (matches the historical
-- semantics: callers prefer the binding body for source snippets).
-- | Parse-tree lookup of one symbol in one file.
--
-- The three outcomes are distinct on purpose.  @Left@ means the module
-- could not be read; @Right Nothing@ means it was read and does not
-- declare the symbol; @Right (Just loc)@ means it does.  Collapsing the
-- first two into @Nothing@ — which is what this function used to do —
-- turns \"unreadable module\" into \"look somewhere else\", and somewhere
-- else is the wrong answer: it is how @Data.Map.Internal.balanceL@ came to
-- report @Data\/Set\/Internal.hs@.
scanFileE :: Text -> FilePath -> IO (Either Parser.ParseError (Maybe SourceLocation))
scanFileE sym f = do
  src <- TIO.readFile f
  pure $ case Parser.parseDecls f src of
    Left e      -> Left e
    Right decls -> Right $ do
      d  <- Parser.findDecl sym decls
      ln <- declAnchor d
      Just (SourceLocation f ln)

-- | The line a declaration is anchored at: its body when it has one, its
-- signature otherwise.  Callers prefer the binding body for source
-- snippets, and a re-export module's entry has only a signature.
declAnchor :: Parser.Decl -> Maybe Int
declAnchor d = case Parser.declDefLine d of
  Just l  -> Just l
  Nothing -> Parser.declSigLine d
