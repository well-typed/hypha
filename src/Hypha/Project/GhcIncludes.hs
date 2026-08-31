{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
-- | Where the plan's compiler keeps the headers its own libraries
-- @#include@.
--
-- @MachDeps.h@ and @ghcplatform.h@ ship with GHC, not with the packages
-- that include them, and cabal puts the compiler's include directory on
-- every CPP invocation.  hypha did not, so every module that asks for
-- @WORD_SIZE_IN_BITS@ or @\<arch\>_HOST_ARCH@ walked into its @#error@
-- arm: 21 of @base-4.16.4.0@'s 251 modules, 20 of
-- @ghc-internal-9.1003.0@'s 233, including the ones a @Data.List@
-- descent passes through.
--
-- The directory is found rather than computed, because its path moved:
-- @\<libdir\>/include@ up to GHC 9.4, and
-- @\<libdir\>/\<platform\>/rts-\<ver\>/include@ from 9.6 on.  Probing for
-- @MachDeps.h@ answers both layouts and any future one that still ships
-- the header somewhere under the libdir.
module Hypha.Project.GhcIncludes
  ( GhcIncludeError (..)
  , renderGhcIncludeError
  , platformIncludeDirs
  , includeDirsUnderLibdir
  ) where

import Control.Monad (filterM)
import Data.List (sort)
import Data.Text (Text)
import Data.Text qualified as Text
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>), takeFileName)

import Hypha.Source.Origins (OriginError, ghcLibdir, renderOriginError)
import Hypha.Types.BuildPlan (CompilerId (..))

-- | Why the compiler's include directory could not be established.
data GhcIncludeError
  = GhcIncludeNoLibdir !CompilerId !OriginError
    -- ^ The plan's compiler could not be reached, or could be and did
    -- not answer @--print-libdir@.  Carries the search's own account.
  | GhcIncludeHeadersMissing !CompilerId !FilePath
    -- ^ A libdir with no @MachDeps.h@ anywhere under it.  Not a
    -- catastrophe and not silence either: the modules that include it
    -- will fail to preprocess, and this is the reason.
  deriving stock (Show, Eq)

-- | Total renderer, for the warning the caller emits.
renderGhcIncludeError :: GhcIncludeError -> Text
renderGhcIncludeError = \case
  GhcIncludeNoLibdir (CompilerId c) e ->
    "no libdir for " <> c <> ": " <> renderOriginError e
  GhcIncludeHeadersMissing (CompilerId c) libdir ->
    "no MachDeps.h under " <> Text.pack libdir <> " (" <> c <> ")"

-- | The include directories of the installation matching @cid@.
--
-- Selected by the plan's compiler version, exactly as the interface
-- reader selects it: a 9.2 project on a 9.10 machine wants 9.2's
-- @MachDeps.h@, whose @SIZEOF_*@ values are the ones its @base@ was
-- built against.
platformIncludeDirs :: CompilerId -> IO (Either GhcIncludeError [FilePath])
platformIncludeDirs cid = ghcLibdir cid >>= \case
  Left e -> pure (Left (GhcIncludeNoLibdir cid e))
  Right libdir -> do
    dirs <- includeDirsUnderLibdir libdir
    pure $ case dirs of
      [] -> Left (GhcIncludeHeadersMissing cid libdir)
      _  -> Right dirs

-- | Every directory under @libdir@ that holds @MachDeps.h@, nearest
-- first.
--
-- Two levels of listing rather than a glob: @\<libdir\>/include@ (GHC
-- <= 9.4), then @\<libdir\>/\<platform\>/rts-\<ver\>/include@ (9.6 and
-- later).  Sorted so the answer does not depend on readdir order — the
-- bug #37 is about.
includeDirsUnderLibdir :: FilePath -> IO [FilePath]
includeDirsUnderLibdir libdir = do
  flat   <- keepWithHeader [libdir </> "include"]
  nested <- nestedRtsIncludes
  pure (flat <> nested)
  where
    nestedRtsIncludes = do
      platforms <- subdirectories libdir
      rtsDirs   <- concat <$> traverse subdirectories platforms
      keepWithHeader [ d </> "include" | d <- rtsDirs, isRts d ]

    isRts = Text.isPrefixOf "rts-" . Text.pack . takeFileName

    subdirectories dir = do
      ok <- doesDirectoryExist dir
      if not ok
        then pure []
        else do
          entries <- sort <$> listDirectory dir
          filterM doesDirectoryExist [ dir </> e | e <- entries ]

    keepWithHeader = filterM (\d -> doesFileExist (d </> "MachDeps.h"))
