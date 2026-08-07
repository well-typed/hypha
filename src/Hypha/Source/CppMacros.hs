{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
-- | The @cabal_macros.h@ hypha synthesises for the C preprocessor.
--
-- When cabal builds a package it generates a header defining
-- @__GLASGOW_HASKELL__@ and a @MIN_VERSION_<pkg>@ macro per dependency,
-- and passes it to every CPP invocation.  hypha reads the same sources
-- without that header, so every conditional was evaluated against an
-- empty macro environment — and in CPP an undefined macro is @0@, which
-- means a module gated on @__GLASGOW_HASKELL__ >= 710@ was indexed from
-- its /pre-7.10/ branch.  The module still parses, still contributes
-- rows, and nothing is reported: a silently wrong answer rather than an
-- absent one.
--
-- Everything here is derived from the build plan, which already carries
-- the compiler and a version per package, so the macros cannot drift
-- from the answers they describe.
module Hypha.Source.CppMacros
  ( CppEnv (..)
  , noCppEnv
  , ghcVersionMacro
  , renderMacroHeader
  , materialiseMacroHeader
  ) where

import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString.Base16 qualified as Base16
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import Text.Read (readMaybe)

import Hypha.Types.PackageId (PackageName (..), Version (..))

-- | What the preprocessor is given for a component.
data CppEnv = CppEnv
  { cppPreInclude  :: !(Maybe FilePath)
    -- ^ A synthesised @cabal_macros.h@, included before the source the
    -- way cabal includes its own.  'Nothing' outside a project, where
    -- there is no plan to derive one from.
  , cppIncludeDirs :: ![FilePath]
    -- ^ Search path for @#include@.  Without it a module including a
    -- header from its package's @include-dirs@ fails outright, and the
    -- whole module is skipped.
  }
  deriving stock (Show, Eq)

-- | No plan, no macros: the honest environment for a source read outside
-- a component context.
noCppEnv :: CppEnv
noCppEnv = CppEnv { cppPreInclude = Nothing, cppIncludeDirs = [] }

-- | @(__GLASGOW_HASKELL__, patch level)@ for a plan's compiler id.
--
-- GHC's encoding is @major * 100 + minor@ — 9.10.3 is @910@, not @9103@
-- — which is what every @#if __GLASGOW_HASKELL__ >= N@ in the ecosystem
-- compares against.
--
-- 'Nothing' for anything unrecognisable, which leaves the macro
-- undefined.  That is the safer failure: an undefined macro sends a gate
-- to its else branch, where a /wrong/ definition would send it
-- confidently to the wrong one.
ghcVersionMacro :: Text -> Maybe (Int, Int)
ghcVersionMacro compilerId = do
  verText <- Text.stripPrefix "ghc-" compilerId
  case traverse (readMaybe . Text.unpack) (Text.splitOn "." verText) of
    Just (major : minor : rest) ->
      Just (major * 100 + minor, case rest of (p : _) -> p; [] -> 0)
    _ -> Nothing

-- | The header text: the compiler macro, then one block per package.
renderMacroHeader :: Text -> [(PackageName, Version)] -> Text
renderMacroHeader compilerId pkgs =
  Text.unlines $
       [ "/* synthesised by hypha from the build plan */" ]
    <> compilerMacros
    <> concatMap packageMacros pkgs
  where
    compilerMacros = case ghcVersionMacro compilerId of
      Nothing -> []
      Just (v, patch) ->
        [ "#define __GLASGOW_HASKELL__ " <> tshow v
        , "#define __GLASGOW_HASKELL_PATCHLEVEL1__ " <> tshow patch
        ]

    packageMacros (PackageName name, Version ver) =
      let ident      = Text.map (\ch -> if ch == '-' then '_' else ch) name
          (a, b, c)  = threeComponents ver
      in [ "/* package " <> name <> "-" <> ver <> " */"
         , "#define VERSION_" <> ident <> " \"" <> ver <> "\""
         , "#define MIN_VERSION_" <> ident <> "(major1,major2,minor) (\\"
         , "  (major1) <  " <> tshow a <> " || \\"
         , "  (major1) == " <> tshow a <> " && (major2) <  " <> tshow b <> " || \\"
         , "  (major1) == " <> tshow a <> " && (major2) == " <> tshow b
             <> " && (minor) <= " <> tshow c <> ")"
         ]

    -- A version may have any number of components; the macro takes
    -- exactly three, so short ones pad with zeroes and long ones are
    -- truncated, matching what cabal writes.
    threeComponents v =
      let ns = [ n | p <- Text.splitOn "." v, Just n <- [readMaybe (Text.unpack p)] ]
      in case ns <> [0, 0, 0] of
           (a : b : c : _) -> (a :: Int, b, c)
           _               -> (0, 0, 0)

    tshow :: Int -> Text
    tshow = Text.pack . show

-- | Write the header under @<cacheRoot>/cpp-macros/@, named by a hash of
-- its contents, and return the path.
--
-- Content-addressed so a plan that has not changed reuses the file and a
-- plan that has gets a different one — there is no staleness question to
-- get wrong, and concurrent runs on different plans cannot collide.
materialiseMacroHeader :: FilePath -> Text -> IO FilePath
materialiseMacroHeader cacheRoot header = do
  let dir  = cacheRoot </> "cpp-macros"
      name = Text.unpack (digest header) <> ".h"
      path = dir </> name
  createDirectoryIfMissing True dir
  exists <- doesFileExist path
  if exists
    then pure path
    else do
      writeFile path (Text.unpack header)
      pure path
  where
    digest =
      Text.decodeUtf8 . Base16.encode . SHA256.hash . Text.encodeUtf8
