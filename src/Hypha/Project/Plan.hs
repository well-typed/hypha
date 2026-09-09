{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Hypha.Project.Plan
  ( -- * Types
    PlanError (..)
  , PlanPin (..)
    -- * Loading
  , loadBuildPlan
  , loadPlanVersions
    -- * Staleness
  , planHash
  ) where

import Control.Applicative ((<|>))
import Control.Exception.Safe (IOException, try)
import Control.Monad (filterM)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString.Base16 as Base16
import Data.List (sort, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Ord (Down (..))
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as TIO

import qualified Cabal.Plan as CP

import Distribution.System (Arch, OS, Platform (..), buildPlatform)
import Distribution.System qualified as System

import Hypha.Cache (sourceCacheRoot)
import qualified Hypha.Hackage.Source as Src
import Hypha.Project.BuildContext (BuildContext (..))
import qualified Hypha.Project.Components as Comp
import qualified Hypha.Project.GhcIncludes as Inc
import qualified Hypha.Source.CppMacros as Cpp
import Hypha.Types.BuildPlan
  ( BuildPlan (..), CompilerId (..), PackageOrigin (..), PlannedUnit (..)
  , ProjectRoot (..) )
import Hypha.Types.PackageId
  ( PackageId (..), PackageName (..), UnitId (..), Version (..) )
import System.Directory
  ( doesDirectoryExist, doesFileExist, getModificationTime, listDirectory )
import System.FilePath
  ( (</>), dropTrailingPathSeparator, normalise, takeDirectory )
import System.IO (hPutStrLn, stderr)

-- | What a plan pins for one package: the version, and the configuration
-- cabal resolved it in.
--
-- Both, because the two answer different questions and the cheap plan
-- reader is the only place that has them side by side: the version is
-- what a user names and what a row is labelled with, the unit-id is what
-- says two rows describe the same build.
data PlanPin = PlanPin
  { ppVersion :: !Version
  , ppUnitId  :: !UnitId
  }
  deriving stock (Show, Eq)

-- | Errors that can occur when loading the build plan.
data PlanError
  = PlanNotFound !FilePath
    -- ^ @plan.json@ does not exist at the expected path.
  | PlanParseFailure !String
    -- ^ The @plan.json@ file could not be decoded.
  deriving stock (Show, Eq)

-- | Load the build plan from @dist-newstyle\/cache\/plan.json@
--   relative to the project root.
--
--   Uses @cabal-plan@'s @findPlanJson@ for robust discovery.  For every
--   unit whose source directory is known (inplace via @pkg-src.path@, a
--   @source-repository-package@ checkout under @dist-newstyle/src@, or
--   resolved through the source cache) we read its @.cabal@ file and
--   stash the list of library components on the 'PlannedUnit'.  This
--   drives sub-library indexing in @hypha server@.
loadBuildPlan :: FilePath -> ProjectRoot -> IO (Either PlanError BuildPlan)
loadBuildPlan cacheRoot (ProjectRoot root) = do
  result <- try @IO @IOException $ do
    planPath <- CP.findPlanJson (CP.ProjectRelativeToDir root)
    (planPath,) <$> CP.decodePlanJson planPath
  case result of
    Left e   -> pure (Left (PlanNotFound (show e)))
    Right (planPath, pj) -> do
      cache <- Src.enumerateSourceCache (sourceCacheRoot cacheRoot)
      -- @<builddir>/cache/plan.json@, so the checkouts are two levels up.
      checkouts <- sourceRepoCheckouts
                     (takeDirectory (takeDirectory planPath) </> "src")
      -- How every module in this plan is to be read: the macros it is
      -- preprocessed against, the compiler headers it may include, and
      -- the platform its stanzas resolve for.  Derived here because this
      -- is the one place that holds the compiler, the target platform and
      -- every package version at once, and derived once rather than per
      -- module.
      ctx   <- buildContextFor cacheRoot pj
      units <- unitsFromPlan pj cache checkouts ctx
      pure (Right (BuildPlan
        { bpCompiler     = compilerFromPlan pj
        , bpUnits        = units
        , bpOverrides    = []
        , bpBuildContext = ctx
        }))

-- | Just what the plan pins, one entry per package: the version and the
-- configuration cabal resolved for it.
--
-- 'loadBuildPlan' resolves a source directory per unit and reads a
-- @.cabal@ file for each one to inventory its components — hundreds of
-- file reads and cabal parses on a real plan.  A caller that only wants
-- to know which versions this project builds against needs none of it,
-- and @hypha lookup@ is on the tier-1 fast path where that work is the
-- dominant cost.  Kept beside 'loadBuildPlan' so the two read the same
-- @plan.json@ through the same discovery, and pinned to agreement by a
-- test rather than by comment.
loadPlanVersions
  :: ProjectRoot -> IO (Either PlanError (Map PackageName PlanPin))
loadPlanVersions (ProjectRoot root) = do
  result <- try @IO @IOException
              (CP.findAndDecodePlanJson (CP.ProjectRelativeToDir root))
  pure $ case result of
    Left e   -> Left (PlanNotFound (show e))
    Right pj -> Right (Map.fromList
      [ ( PackageName name
        , PlanPin (Version (CP.dispVer ver)) (unitIdOf u) )
      -- Same ordering as 'unitsFromPlan': a package contributes one unit
      -- per component, 'Map.fromList' keeps the last, and the
      -- library-carrying unit is the one whose id the cache is keyed on.
      -- Without the sort this reader pins @mylib-0.1.0-inplace-myexe@
      -- while the indexer writes under @mylib-0.1.0-inplace@, and every
      -- local package looks stale to the tier it is meant to warm.
      | u <- libraryLast (Map.elems (CP.pjUnits pj))
      , let CP.PkgId (CP.PkgName name) ver = CP.uPId u
      ])

-- | Everything the plan settles about reading its packages' sources.
--
-- Both halves are best-effort and neither is silent: without the macro
-- header every version gate resolves to its oldest branch, and without
-- the compiler's include directory every module that includes
-- @MachDeps.h@ fails to preprocess.  The platform is the plan's own,
-- falling back to this machine's when @plan.json@ names one we do not
-- recognise.
buildContextFor :: FilePath -> CP.PlanJson -> IO BuildContext
buildContextFor cacheRoot pj = do
  mMacroHeader <- macroHeaderFor cacheRoot pj
  includes     <- platformIncludesFor (compilerFromPlan pj)
  pure BuildContext
    { bcCpp = Cpp.CppEnv
        { Cpp.cppPreInclude  = mMacroHeader
        , Cpp.cppIncludeDirs = includes
        }
    , bcPlatform = platformFromPlan pj
    }

-- | The compiler's own header directories, or @[]@ with the reason said
-- out loud.
platformIncludesFor :: CompilerId -> IO [FilePath]
platformIncludesFor cid = do
  r <- Inc.platformIncludeDirs cid
  case r of
    Right dirs -> pure dirs
    Left err   -> do
      hPutStrLn stderr $
        "warning: GHC's own headers unavailable ("
        <> Text.unpack (Inc.renderGhcIncludeError err)
        <> "); modules that #include MachDeps.h or ghcplatform.h \
           \will not preprocess"
      pure []

-- | The platform @plan.json@ was solved for.
--
-- cabal writes @arch@ and @os@ as the strings its own parser reads, so
-- they round-trip; an unrecognised one means a newer cabal than the one
-- we link against, and this machine's platform is a better answer than
-- refusing to resolve any @os()@ condition at all.
platformFromPlan :: CP.PlanJson -> Platform
platformFromPlan pj = Platform arch os
  where
    Platform hostArch hostOs = buildPlatform

    arch :: Arch
    arch = case System.classifyArch System.Permissive (Text.unpack (CP.pjArch pj)) of
      System.OtherArch _ -> hostArch
      a                  -> a

    os :: OS
    os = case System.classifyOS System.Permissive (Text.unpack (CP.pjOs pj)) of
      System.OtherOS _ -> hostOs
      o                -> o

-- | Write the @cabal_macros.h@ this plan implies, and return its path.
--
-- Best-effort: a cache root that cannot be written is not a reason to
-- fail loading a plan, but it is a reason to say so — without the header
-- every version gate silently resolves to its oldest branch.
macroHeaderFor :: FilePath -> CP.PlanJson -> IO (Maybe FilePath)
macroHeaderFor cacheRoot pj = do
  let CompilerId compiler = compilerFromPlan pj
      pkgs =
        [ (PackageName name, Version (CP.dispVer ver))
        | u <- Map.elems (CP.pjUnits pj)
        , let CP.PkgId (CP.PkgName name) ver = CP.uPId u
        ]
      header = Cpp.renderMacroHeader compiler pkgs
  r <- try @IO @IOException (Cpp.materialiseMacroHeader cacheRoot header)
  case r of
    Right path -> pure (Just path)
    Left err   -> do
      hPutStrLn stderr $
        "warning: CPP macros unavailable (" <> show err
        <> "); modules guarded by #if __GLASGOW_HASKELL__ or MIN_VERSION_* \
           \will be read from their oldest branch"
      pure Nothing

-- | Units ordered so a package's library-carrying one comes last.
--
-- Shared by both plan readers, which is the point: it decides which unit
-- represents a package, and two answers to that is what the agreement
-- test exists to catch.
libraryLast :: [CP.Unit] -> [CP.Unit]
libraryLast = sortOn (\u -> CP.CompNameLib `Map.member` CP.uComps u)

-- | cabal's unit-id for a plan unit, as text.
--
-- @cabal-plan@ wraps it in its own newtype; ours is the one the rest of
-- the codebase and the cache schema speak, and the conversion happens
-- here so it happens once.
unitIdOf :: CP.Unit -> UnitId
unitIdOf u = let CP.UnitId t = CP.uId u in UnitId t

-- | Extract compiler identifier from the plan.
compilerFromPlan :: CP.PlanJson -> CompilerId
compilerFromPlan pj =
  let CP.PkgId (CP.PkgName name) (CP.Ver parts) = CP.pjCompilerId pj
      verStr = Text.intercalate (Text.pack ".") (map (Text.pack . show) parts)
  in CompilerId (name <> Text.pack "-" <> verStr)

-- | Extract planned units with their dependencies from the plan.
unitsFromPlan
  :: CP.PlanJson
  -> Map FilePath FilePath
     -- ^ @\"pkg-ver\" -> sourceDir@ for dependency packages, from the
     -- source cache.
  -> [FilePath]
     -- ^ Every @source-repository-package@ checkout directory cabal has
     -- made for this project; see 'sourceRepoCheckouts'.
  -> BuildContext
     -- ^ How this plan's sources are read: CPP environment + platform.
  -> IO (Map PackageName PlannedUnit)
unitsFromPlan pj sourceCacheLookup checkouts ctx = do
      -- A package contributes one unit per component (lib, exes, test
      -- suites).  The map below is keyed by package *name* and
      -- 'Map.fromList' retains the last duplicate, so order units
      -- with the library-carrying one last: it is the unit whose
      -- dist-dir holds the rendered Haddock and whose dependencies
      -- describe the library.  Without this, hypha's own test-suite
      -- unit used to win and @puDistDir@ pointed at @t/<pkg>-tests@.
  let allUnits = libraryLast (Map.elems (CP.pjUnits pj))
      unitIdToPkgId = Map.fromList
        [ (CP.uId u, CP.uPId u) | u <- allUnits ]
  pairs <- mapM
    (\u -> do
       pu <- toPlannedUnit unitIdToPkgId sourceCacheLookup checkouts ctx u
       let CP.PkgId (CP.PkgName pkgText) _ = CP.uPId u
       pure (PackageName pkgText, pu))
    allUnits
  pure (Map.fromList pairs)

-- | Convert a cabal-plan Unit to our PlannedUnit type.
toPlannedUnit
  :: Map CP.UnitId CP.PkgId
  -> Map FilePath FilePath
  -> [FilePath]
  -> BuildContext
  -> CP.Unit
  -> IO PlannedUnit
toPlannedUnit unitIdToPkgId sourceCacheLookup checkouts ctx u = do
  let CP.PkgId (CP.PkgName name) ver = CP.uPId u
      pkgId   = PackageId (PackageName name) (Version (CP.dispVer ver))
      libDeps = concatMap (Set.toList . CP.ciLibDeps) (Map.elems (CP.uComps u))
      deps    = [ toPackageId pid
                | uid <- libDeps
                , Just pid <- [Map.lookup uid unitIdToPkgId]
                ]
      depKey  = Text.unpack name <> "-" <> Text.unpack (CP.dispVer ver)
  -- Where the unit's source is, when it is on disk somewhere we know.
  -- Tarballs of every kind are not: they go through the source cache.
  srcDir <- case CP.uPkgSrc u of
    Nothing                               -> pure Nothing
    Just (CP.LocalUnpackedPackage p)      -> pure (Just p)
    Just (CP.LocalTarballPackage _)       -> pure Nothing
    Just (CP.RemoteTarballPackage _)      -> pure Nothing
    Just (CP.RepoTarballPackage _)        -> pure Nothing
    Just (CP.RemoteSourceRepoPackage sr)  ->
      locateSourceRepoCheckout checkouts (Text.unpack name) sr
  let
      sourceDir = case srcDir of
        Just d  -> Just d
        Nothing -> Map.lookup depKey sourceCacheLookup
  comps <- case sourceDir of
    Just d  -> componentsFor d ctx
    Nothing -> pure []
  pure PlannedUnit
    { puId            = pkgId
    , puUnitId        = unitIdOf u
    , puDeps          = deps
    , puIsLocal       = (CP.uType u == CP.UnitTypeLocal)
    , puOrigin        = originFromPkgLoc (CP.uPkgSrc u)
    , puSrcDir        = srcDir
    , puDistDir       = CP.uDistDir u
    , puLibComponents = comps
    }

-- | Parse the @.cabal@ file in a directory and return its library
-- components.  Silent fallback to @[]@ on any kind of failure.
componentsFor :: FilePath -> BuildContext -> IO [Comp.ComponentInfo]
componentsFor d ctx = do
  mCabal <- Comp.findCabalFile d
  case mCabal of
    Just c  -> Comp.parseLibComponents c d ctx
    Nothing -> pure []

-- | Every directory cabal has checked a @source-repository-package@ out
-- into: the subdirectories of @<builddir>/src@.  Listed once per plan
-- load and shared by every unit, since a real project has hundreds of
-- entries there.  The directory is named after the /repository/ plus a
-- hash, so a unit cannot be found by name; see
-- 'locateSourceRepoCheckout'.
sourceRepoCheckouts :: FilePath -> IO [FilePath]
sourceRepoCheckouts srcRoot = do
  -- A project without SRPs has no such directory; that is not a failure.
  exists <- doesDirectoryExist srcRoot
  if not exists
    then pure []
    else do
      r <- try @IO @IOException (listDirectory srcRoot)
      case r of
        Right entries -> filterM doesDirectoryExist (map (srcRoot </>) entries)
        Left e        -> do
          hPutStrLn stderr $
            "warning: cannot list source-repository checkouts under "
              <> srcRoot <> ": " <> show e
              <> "; their packages will resolve as if never checked out"
          pure []

-- | The checkout a @source-repository-package@ unit builds from.
--
-- A checkout qualifies when its package root — the repository's
-- @subdir@, if the stanza names one — holds @<pkg>.cabal@.  cabal
-- re-checks a repository out every time the stanza's tag moves and never
-- removes the old copies, so several may qualify: the one whose git
-- @HEAD@ resolves to the plan's commit wins, and among those (or when
-- none does) the most recently modified repository root.
locateSourceRepoCheckout
  :: [FilePath] -> String -> CP.SourceRepo -> IO (Maybe FilePath)
locateSourceRepoCheckout checkouts name sr = do
  -- Normalised so a @subdir@ of @haskell-bee/@ or @./pkgs/x@ yields the
  -- same path the plain form does.
  let pkgDir repo = dropTrailingPathSeparator
                      (normalise (repo </> fromMaybe "" (CP.srSubdir sr)))
      roots = [ (repo, pkgDir repo) | repo <- checkouts ]
  matching <- filterM (\(_, d) -> doesFileExist (d </> name <> ".cabal")) roots
  ranked   <- mapM rank matching
  pure (fst <$> listToMaybe (sortOn snd ranked))
  where
    -- Sort key: pinned-commit matches first, then newest first.  The
    -- repository root's mtime, not the package dir's: git only bumps a
    -- directory when an entry is added or removed under it.
    rank (repo, d) = do
      atPin <- case CP.srTag sr of
        Nothing  -> pure False
        Just tag -> (== Just (Text.strip tag)) <$> gitHeadCommit repo
      mtime <- getModificationTime repo
      pure (d, (Down atPin, Down mtime))

-- | The commit a git checkout is at, or 'Nothing' when the directory is
-- not one (a tarball unpack, another VCS).
--
-- cabal's sync leaves @HEAD@ as a symbolic ref, so the commit is in the
-- named ref, loose under @.git/refs@ or in @.git/packed-refs@ after a gc.
-- A bare hash in @HEAD@ is a detached checkout.
gitHeadCommit :: FilePath -> IO (Maybe Text)
gitHeadCommit repo = runMaybeT $ do
  headTxt <- MaybeT (gitFile "HEAD")
  case Text.stripPrefix "ref: " headTxt of
    Nothing  -> pure headTxt
    Just ref -> MaybeT (gitFile (Text.unpack ref)) <|> MaybeT (packedRef ref)
  where
    gitFile rel = readIfExists (repo </> ".git" </> rel)
    packedRef ref = do
      packed <- readIfExists (repo </> ".git" </> "packed-refs")
      pure $ listToMaybe
        [ commit
        | line <- maybe [] Text.lines packed
        , [commit, r] <- [Text.words line]
        , r == ref
        ]

-- | A file's stripped text, 'Nothing' when there is no such file.  A file
-- that exists but will not read is reported, since the caller cannot
-- tell that apart from absence and would pick another checkout on it.
readIfExists :: FilePath -> IO (Maybe Text)
readIfExists f = do
  exists <- doesFileExist f
  if not exists
    then pure Nothing
    else do
      r <- try @IO @IOException (TIO.readFile f)
      case r of
        Right t -> pure (Just (Text.strip t))
        Left e  -> do
          hPutStrLn stderr ("warning: cannot read " <> f <> ": " <> show e)
          pure Nothing

-- | Map a @cabal-plan@ source location to our coarser 'PackageOrigin'.
-- We do not surface 'OriginSourceRepo' metadata that the plan omits;
-- if cabal didn't record a URL, neither do we.
originFromPkgLoc :: Maybe CP.PkgLoc -> PackageOrigin
originFromPkgLoc = \case
  Nothing -> OriginDistribution
  Just (CP.LocalUnpackedPackage p) -> OriginLocal p
  Just (CP.LocalTarballPackage  p) -> OriginLocalTarball p
  Just (CP.RemoteTarballPackage (CP.URI u)) -> OriginRemoteTarball u
  Just (CP.RepoTarballPackage _)   -> OriginHackage
  Just (CP.RemoteSourceRepoPackage sr) ->
    OriginSourceRepo
      (CP.srLocation sr)
      -- Prefer explicit tag, fall back to branch — cabal stores the
      -- resolved commit hash in @tag@ for @source-repository-package@
      -- pinned via @tag:@ but in @branch@ when only a branch is given.
      (CP.srTag sr <|> CP.srBranch sr)
      (CP.srSubdir sr)

-- | Convert a cabal-plan PkgId to our PackageId type.
toPackageId :: CP.PkgId -> PackageId
toPackageId (CP.PkgId (CP.PkgName name) ver) =
  PackageId (PackageName name) (Version (CP.dispVer ver))

-- | SHA-256 (hex) over the in-memory plan.  Used as the staleness
-- stamp for the local Hoogle DB: when the set of pinned
-- @(pkg, version)@ pairs changes, the hash changes too.  We hash the
-- canonical form rather than the on-disk @plan.json@ so that
-- semantically identical plans produce identical hashes.
planHash :: BuildPlan -> Text
planHash bp =
  let pids = sort
        [ unPackageName (pkgName (puId u))
          <> "-"
          <> unVersion (pkgVersion (puId u))
        | u <- Map.elems (bpUnits bp)
        ]
      payload = Text.encodeUtf8 (Text.unlines pids)
      digest  = SHA256.hash payload
  in Text.decodeUtf8 (Base16.encode digest)
