{-# LANGUAGE OverloadedStrings #-}
-- | Loader for the @test/fixtures/reexport@ component, shared by the
-- index, locate and module-doc suites so they cannot drift apart on what
-- the fixture contains.
module Util.Fixture
  ( fixtureSources
  , sourcesFor
  ) where

import           Data.Text (Text)
import qualified Data.Text.IO as TIO

import Hypha.Search.Index (ModuleSource (..), Visibility (..))
import Hypha.Types.SymbolPath (ModulePath (..))

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
fixtureSources = sourcesFor
  [ ("test/fixtures/reexport/src/Fixture/Internal.hs",       "Fixture.Internal",       Exposed)
  , ("test/fixtures/reexport/src/Fixture/Facade.hs",         "Fixture.Facade",         Exposed)
  , ("test/fixtures/reexport/src/Fixture/Wrapper.hs",        "Fixture.Wrapper",        Exposed)
  , ("test/fixtures/reexport/src/Fixture/Strict.hs",         "Fixture.Strict",         Exposed)
  , ("test/fixtures/reexport/src/Fixture/StrictInternal.hs", "Fixture.StrictInternal", Internal)
  , ("test/fixtures/reexport/src/Fixture/Other.hs",          "Fixture.Other",          Exposed)
  , ("test/fixtures/reexport/src/Fixture/Renamed.hs",        "Fixture.Declared",       Internal)
  ]
