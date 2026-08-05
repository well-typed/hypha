{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
-- | How far outside its own component a resolution pass can see.
--
-- Resolving a re-export stops being a component-local question the moment
-- the chain leaves the component: @base@'s @Data.List@ passes @sortOn@ to
-- @GHC.Internal.Data.List@, which passes it to
-- @GHC.Internal.Data.OldList@, and both live in @ghc-internal@.  This is
-- the handle "Hypha.Source.Locate" follows such a chain through.
--
-- A record of functions rather than a table, because the two producers
-- reach outside by different means and only one of them can enumerate
-- what it will need.  @hypha server@ preloads exactly the modules its
-- index named ('reachFrom'); the CLI has no index, so it consults a
-- dependency only once a candidate names a module in it
-- ("Hypha.Source.Dependencies") — a table would mean unpacking every
-- dependency of the package being asked about before answering anything.
module Hypha.Source.Reach
  ( OutsideReach (..)
  , OutsideModule (..)
  , noOutsideReach
  , reachFrom
    -- * What a reach could not read
  , ReachGap (..)
  , renderReachGap
    -- * How locating a symbol can fail
  , SymbolSearchFailure (..)
  , renderSymbolSearchFailure
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text

import Hypha.Search.Index
  ( DefinitionRef (..), ImportedDefinitions (..), ModuleSource )
import Hypha.Source.Extensions (LanguageSettings)
import Hypha.Source.Origins (OriginError, renderOriginError)
import Hypha.Types.ComponentName (ComponentKey)
import Hypha.Types.PackageId
  (PackageId, PackageName (..), renderPackageId)
import Hypha.Types.SymbolPath
  (ModulePath (..), SymbolName (..))

-- | A module the asking component does not own.
--
-- The language settings travel with the source because they belong to the
-- stanza the module came from, not to the stanza that asked for it.
-- Parsing a dependency's module under the /asking/ component's
-- @default-extensions@ is the same defect
-- 'Hypha.Source.Locate.locateDefinitionInComponent' documents fixing one
-- hop earlier, and it reappears across a package boundary unless the
-- settings are carried here.
data OutsideModule = OutsideModule
  { omComponent :: !ComponentKey
  , omSource    :: !ModuleSource
  , omLanguage  :: !LanguageSettings
  }
  deriving stock (Show, Eq)

-- | What a resolution pass can see beyond its own component.
data OutsideReach m = OutsideReach
  { orSites  :: !(Map SymbolName DefinitionRef)
    -- ^ Definition sites something else already resolved transitively.
    --
    -- Can be missing a name the asking module exports — the index may
    -- still be building, class methods have no rows at all, and it is
    -- empty outright for the CLI — so consumers fall back to resolving
    -- through imports and must never read a miss as \"no such symbol\".
  , orModule :: !(ModulePath -> m (Maybe OutsideModule))
    -- ^ The source of a module outside the asking component.
    --
    -- 'Nothing' means no component within reach declares that module
    -- name.  It does /not/ mean the module does not exist: a reach built
    -- from a plan can only see dependencies whose sources are on disk.
    -- 'orGaps' says which ones were not, so a search that came up short
    -- for want of a source is never reported as one that came up short
    -- because the symbol is absent.
  , orGaps   :: !(m [ReachGap])
    -- ^ Everything this reach could not read, accumulated as it went.
    --
    -- Read at the point a failure is reported rather than warned about
    -- when it happens: a dependency the chain never needed is not a
    -- reason for anything, and announcing it beside a correct answer
    -- invents a causal link that is not there.
  }

-- | No dependency graph to resolve through: every lookup misses, and
-- nothing was attempted, so there is nothing to report either.
noOutsideReach :: Applicative m => OutsideReach m
noOutsideReach = OutsideReach
  { orSites  = Map.empty
  , orModule = const (pure Nothing)
  , orGaps   = pure []
  }

-- | Everything the caller already holds, and nothing beyond it.
--
-- The settings function is a parameter because a preloaded module belongs
-- to a component the caller can look up and this module cannot: only the
-- caller has the plan.
reachFrom
  :: Applicative m
  => (ComponentKey -> LanguageSettings)
  -> ImportedDefinitions
  -> OutsideReach m
reachFrom langsOf imported = OutsideReach
  { orSites  = idSites imported
  , orModule = \m -> pure (outsideModule <$> Map.lookup m (idSources imported))
  , orGaps   = pure []
  }
  where
    outsideModule (comp, ms) = OutsideModule
      { omComponent = comp
      , omSource    = ms
      , omLanguage  = langsOf comp
      }

-- | Something a reach could not read, and therefore could not offer.
--
-- Carried as the packages and paths involved rather than as a sentence,
-- so the wire layer decides how to say it and a caller can still tell the
-- three cases apart.
data ReachGap
    -- | The plan has no unit for a package, so its dependencies are
    -- unknown.  Nothing outside it can be reached.
  = GapUnitNotInPlan !PackageName
    -- | A dependency with no unpacked source on disk.  Deliberately not
    -- fetched: probing a candidate module name against every dependency
    -- of the package is speculative, and a tarball download per probe is
    -- not a proportionate price for a guess.  The one unit that
    -- /does/ own the module being looked for is fetched instead; see
    -- 'GapOwnerUnfetchable' and 'GapOwnerUnaskable' for that path failing.
  | GapNoLocalSource !PackageId
    -- | A dependency whose cabal file named no library, so its modules
    -- had to be guessed at by walking directories.
  | GapNoModuleList  !PackageId !FilePath
    -- | The unit that owns a needed module was identified, and its source
    -- could not be materialised.  The resolver's own error is reported
    -- where it happens — this records that the owner was known and the
    -- fetch is why the chain still stopped.
  | GapOwnerUnfetchable !PackageId !ModulePath
    -- | Which unit owns a module could not be established at all, so the
    -- walk had only the unpacked dependencies to go on.  Carries the
    -- compiler's own reason.
  | GapOwnerUnaskable !OriginError
  deriving stock (Show, Eq)

-- | Why looking for a symbol's definition did not reach one.
--
-- Several outcomes, not one, because a caller has to tell them apart.  A
-- search that stopped at a limit has not established that the symbol is
-- absent — it established that we stopped looking — and reporting both as
-- \"not found\" tells the reader the opposite of the truth and gives an
-- agent nothing to retry on.
--
-- Carried as the modules and counts involved rather than as a sentence:
-- 'renderSymbolSearchFailure' is one consumer, the envelope's @actions@
-- map is another, and neither should have to parse prose the search
-- already knew the structure of.
data SymbolSearchFailure
    -- | The asking module does not export the symbol at all, so there was
    -- no chain to follow.
  = SearchNotExported
    -- | The chain was followed to exhaustion.  These are the modules the
    -- first ring offered; none of the ones we could read declared it.
  | SearchNoSupplier ![ModulePath]
    -- | The resolution named a module of the asking component as the
    -- definition site, but that module's parse declares no such name.
  | SearchNotDeclared !ModulePath
    -- | The resolution named a defining module the component's own parse
    -- did not produce.
  | SearchModuleUnparsed !ModulePath
    -- | The chain is longer than the search follows.  Carries the limit
    -- and the frontier it stopped at.
  | SearchHopLimit !Int ![ModulePath]
    -- | The search ran out of parses before it ran out of candidates.
  | SearchParseBudget !Int
    -- | The package has no readable library stanza, so no export could be
    -- resolved and the only option left was scanning its files — which
    -- found no such binding.  Carries the directory scanned.
  | SearchSweptPackage !FilePath
    -- | The package's own components are readable and none of them has
    -- the module, nor is there a file for it under the source directory.
    -- The module does not exist in this package, so there is no chain to
    -- follow and nothing to scan for: a misspelling answered by a
    -- package-wide scan comes back as a confident wrong answer under the
    -- name that was asked for.
  | SearchModuleAbsent
  deriving stock (Show, Eq)

-- | Say what happened, given the module and symbol the caller asked
-- about.  The failure carries no names of its own for those two: the
-- caller has them, and duplicating them would let the two disagree.
renderSymbolSearchFailure
  :: ModulePath -> SymbolName -> SymbolSearchFailure -> Text
renderSymbolSearchFailure asking sym failure = case failure of
  SearchNotExported ->
    unModulePath asking <> " does not export '" <> unSymbolName sym <> "'"
  SearchNoSupplier cands ->
    reexports <> ", and none of the modules that could supply it both"
      <> " provided a source and declared it: " <> renderModules cands
  SearchNotDeclared m ->
    reexports <> " from " <> unModulePath m
      <> ", which declares no such name"
  SearchModuleUnparsed m ->
    reexports <> " from " <> unModulePath m
      <> ", which is not among the modules of the component we could parse"
  SearchHopLimit limit frontier ->
    reexports <> ", and the chain is longer than " <> tshow limit
      <> " hops; the search stopped at " <> renderModules frontier
  SearchParseBudget budget ->
    reexports <> ", and reaching it would take more than " <> tshow budget
      <> " module parses; the search stopped there"
  SearchSweptPackage dir ->
    "no library stanza under " <> Text.pack dir <> " lists "
      <> unModulePath asking <> ", and scanning the package found no '"
      <> unSymbolName sym <> "'"
  SearchModuleAbsent ->
    "no component of the package has " <> unModulePath asking
      <> ", and no file for it exists under its source directory"
  where
    reexports =
      unModulePath asking <> " re-exports '" <> unSymbolName sym <> "'"

    renderModules = Text.intercalate ", " . map unModulePath

    tshow :: Int -> Text
    tshow = Text.pack . show

renderReachGap :: ReachGap -> Text
renderReachGap gap = case gap of
  GapUnitNotInPlan n ->
    "the build plan has no unit for '" <> unPackageName n
      <> "', so its dependencies are unknown and a re-export leaving it"
      <> " cannot be followed"
  GapNoLocalSource pid ->
    renderPackageId pid <> " has no unpacked source on disk, so it could"
      <> " not be searched for a module without fetching it; only the unit"
      <> " that owns a needed module is fetched"
  GapNoModuleList pid dir ->
    renderPackageId pid <> " has no readable library stanza under "
      <> Text.pack dir <> ", so its module list was guessed by walking"
      <> " directories and may be incomplete"
  GapOwnerUnfetchable pid m ->
    renderPackageId pid <> " owns " <> unModulePath m
      <> " and its source could not be fetched; `cabal get "
      <> renderPackageId pid <> "` puts it where hypha looks"
  GapOwnerUnaskable err ->
    "which package owns a module could not be established, so only"
      <> " dependencies already unpacked were searched: "
      <> renderOriginError err
