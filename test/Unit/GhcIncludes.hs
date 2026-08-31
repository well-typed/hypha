{-# LANGUAGE OverloadedStrings #-}
-- | Finding the compiler's own header directory, and what having it does
-- to a module that includes @MachDeps.h@.
module Unit.GhcIncludes (tests) where

import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import Hypha.Project.GhcIncludes (includeDirsUnderLibdir)
import Hypha.Source.CppMacros (CppEnv (..), noCppEnv)
import Hypha.Source.Extensions (LanguageSettings (..), defaultLanguageSettings)
import Hypha.Source.Parser (ParseError, parseErrorMessage, parseModuleWith)

-- | @<libdir>/include/MachDeps.h@ — GHC 9.4 and earlier.
flatLayout :: FilePath -> IO FilePath
flatLayout root = do
  let libdir = root </> "lib" </> "ghc-9.2.8"
  writeHeader (libdir </> "include")
  pure libdir

-- | @<libdir>/<platform>/rts-<ver>/include/MachDeps.h@ — GHC 9.6 on.
nestedLayout :: FilePath -> IO FilePath
nestedLayout root = do
  let libdir = root </> "lib" </> "ghc-9.10.3" </> "lib"
  writeHeader (libdir </> "x86_64-linux-ghc-9.10.3" </> "rts-1.0.2" </> "include")
  -- A sibling that is not the RTS, and a sibling package that is not a
  -- directory of headers: neither may be mistaken for the answer.
  createDirectoryIfMissing True (libdir </> "x86_64-linux-ghc-9.10.3" </> "base-4.20.2.0")
  pure libdir

writeHeader :: FilePath -> IO ()
writeHeader dir = do
  createDirectoryIfMissing True dir
  TIO.writeFile (dir </> "MachDeps.h") "#define WORD_SIZE_IN_BITS 64\n"

-- | A module that cannot be preprocessed without @MachDeps.h@ — the
-- shape @GHC.Event.Array@ and 20 of its neighbours have.
gatedModule :: Text.Text
gatedModule = Text.unlines
  [ "{-# LANGUAGE CPP #-}"
  , "module Fixture.Gated (wordSize) where"
  , "#include \"MachDeps.h\""
  , "#if WORD_SIZE_IN_BITS == 64"
  , "wordSize :: Int"
  , "wordSize = 64"
  , "#elif WORD_SIZE_IN_BITS == 32"
  , "wordSize :: Int"
  , "wordSize = 32"
  , "#else"
  , "#error firstPowerOf2 not defined on this architecture"
  , "#endif"
  ]

parseWith :: [FilePath] -> Either ParseError ()
parseWith dirs =
  () <$ parseModuleWith
          defaultLanguageSettings
            { lsCpp = noCppEnv { cppIncludeDirs = dirs } }
          "Fixture/Gated.hs"
          gatedModule

tests :: TestTree
tests = testGroup "Unit.GhcIncludes"
  [ testCase "the pre-9.6 layout is found" $
      withSystemTempDirectory "hypha-inc" $ \tmp -> do
        libdir <- flatLayout tmp
        dirs   <- includeDirsUnderLibdir libdir
        dirs @?= [libdir </> "include"]

  , testCase "the 9.6-and-later layout is found" $
      withSystemTempDirectory "hypha-inc" $ \tmp -> do
        libdir <- nestedLayout tmp
        dirs   <- includeDirsUnderLibdir libdir
        dirs @?=
          [ libdir </> "x86_64-linux-ghc-9.10.3" </> "rts-1.0.2" </> "include" ]

  , testCase "a libdir without the header yields nothing" $
      withSystemTempDirectory "hypha-inc" $ \tmp -> do
        createDirectoryIfMissing True (tmp </> "include")
        dirs <- includeDirsUnderLibdir tmp
        dirs @?= []

  , testCase "a libdir that does not exist yields nothing" $
      withSystemTempDirectory "hypha-inc" $ \tmp -> do
        dirs <- includeDirsUnderLibdir (tmp </> "absent")
        dirs @?= []

  , testCase "without the header dir the module cannot be preprocessed" $
      case parseWith [] of
        Right () -> fail "expected the #error arm to be taken"
        Left err ->
          assertBool ("unexpected message: " <> show (parseErrorMessage err))
            ("MachDeps.h" `Text.isInfixOf` parseErrorMessage err
              || "firstPowerOf2" `Text.isInfixOf` parseErrorMessage err)

  , testCase "with the header dir on the path it parses" $
      withSystemTempDirectory "hypha-inc" $ \tmp -> do
        libdir <- flatLayout tmp
        dirs   <- includeDirsUnderLibdir libdir
        case parseWith dirs of
          Right () -> pure ()
          Left err -> fail ("expected a parse, got: "
                             <> Text.unpack (parseErrorMessage err))
  ]
