{-# LANGUAGE OverloadedStrings #-}
-- | Loader for the @test/fixtures/reexport@ component, shared by the
-- index, locate and module-doc suites so they cannot drift apart on what
-- the fixture contains.
--
-- The module list and the visibilities come from the fixture's own cabal
-- file, through the same 'Indexer.packageSources' the CLI uses: a
-- hardcoded copy here would have been a second declaration of the same
-- facts, free to disagree with the file it mirrors.  It also means the
-- cabal-reading path is exercised end to end rather than only in
-- isolation.
module Util.Fixture
  ( fixtureSources
  , depSources
  , sourcesFor
  ) where

import           Data.Text (Text)
import qualified Data.Text.IO as TIO

import Hypha.Search.Index (ModuleSource (..), Visibility (..))
import Hypha.Search.Indexer (packageSources)
import Hypha.Types.SymbolPath (ModulePath (..))

-- | Load explicitly named sources, for the fixtures a cabal file cannot
-- describe: a module whose file name disagrees with its header, or one
-- that only some suites want in the component.
sourcesFor :: [(FilePath, Text, Visibility)] -> IO [ModuleSource]
sourcesFor = mapM $ \(fp, declared, vis) -> do
  content <- TIO.readFile fp
  pure ModuleSource
    { msDeclaredName = ModulePath declared
    , msPath         = fp
    , msVisibility   = vis
    , msContent      = content
    }

-- | The fixture component, with the visibility its cabal file declares.
--
-- @Fixture.Internal@ is deliberately exposed: that is the @containers@
-- situation, where an @.Internal@ module is public and must still lose to
-- the wrapper that documents it.
fixtureSources :: IO [ModuleSource]
fixtureSources = componentSources "test/fixtures/reexport"

-- | The @reexport-dep@ component: the package @Fixture.Imported@
-- re-exports from.  Kept separate from 'fixtureSources' because the whole
-- point of the fixture is that the two are different components.
depSources :: IO [ModuleSource]
depSources = componentSources "test/fixtures/reexport-dep"

componentSources :: FilePath -> IO [ModuleSource]
componentSources root = concatMap snd <$> packageSources root
