{-# LANGUAGE OverloadedStrings #-}
-- | Coverage for 'Hypha.Source.Origins': reading a module's export
-- origins out of GHC's own compiled interface.
--
-- The resolver can only rank imports; it cannot know which one supplies
-- a name, because an open @import Prelude@ supplies every name
-- syntactically and none of them in fact.  GHC already knows — it ran the
-- renamer — and it writes the answer into the @.hi@ file, one
-- fully-qualified origin per export.  Everything below pins a shape that
-- appears in a real @ghc --show-iface@ dump of @base@.
module Unit.SourceOrigins (tests) where

import           Control.Exception (IOException, try)
import           Data.List (isPrefixOf)
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import qualified Data.Text       as Text
import qualified Data.Text.IO    as TIO
import           Data.Version (showVersion)
import           System.Directory (doesFileExist, listDirectory)
import           System.FilePath ((</>))
import           System.Info (fullCompilerVersion)

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import Hypha.Source.Origins
  ( ModuleOrigins (..), OriginOracle (..), discoverPackageDbs
  , mkGhcOriginOracle, parseShowIface )
import Hypha.Types.BuildPlan (CompilerId (..))
import Hypha.Types.PackageId (PackageId (..), PackageName (..), Version (..))
import Hypha.Types.SymbolPath (ModulePath (..), SymbolName (..))

-- | The origins of one captured dump, or a failure the case can report.
originsOf :: FilePath -> IO (Map.Map SymbolName (NonEmpty ModulePath))
originsOf fp = do
  dump <- TIO.readFile fp
  case parseShowIface dump of
    Left e   -> fail ("parseShowIface failed for " <> fp <> ": " <> show e)
    Right mo -> pure (moOrigins mo)

concurrent :: IO (Map.Map SymbolName (NonEmpty ModulePath))
concurrent = originsOf "test/fixtures/iface/Control.Concurrent.showiface"

bits :: IO (Map.Map SymbolName (NonEmpty ModulePath))
bits = originsOf "test/fixtures/iface/Data.Bits.showiface"

-- | One origin, the common case.
only :: Text.Text -> Maybe (NonEmpty ModulePath)
only m = Just (ModulePath m :| [])

tests :: TestTree
tests = testGroup "Unit.SourceOrigins"
  [ testCase "a qualified export names the module that defines it" $ do
      -- The symbol this whole exercise started from: exported by
      -- base:Control.Concurrent, declared in ghc-internal, and blamed on
      -- Prelude by anything that reads only the import list.
      m <- concurrent
      Map.lookup (SymbolName "isCurrentThreadBound") m
        @?= only "GHC.Internal.Conc.Bound"

  , testCase "an unqualified export belongs to the module itself" $ do
      -- GHC prints the origin only when it differs from the interface's
      -- own module, so a bare name is a local declaration.
      m <- concurrent
      Map.lookup (SymbolName "forkFinally") m @?= only "Control.Concurrent"

  , testCase "a class brings its methods, each with its own origin" $ do
      -- Bits{.&. .|. ...}: the methods are exports too, and they are
      -- exactly what the source parser cannot see (issue 043).
      m <- bits
      Map.lookup (SymbolName "Bits") m   @?= only "GHC.Internal.Bits"
      Map.lookup (SymbolName ".&.") m    @?= only "GHC.Internal.Bits"
      Map.lookup (SymbolName "shiftL") m @?= only "GHC.Internal.Bits"

  , testCase "an operator name keeps the dots that belong to it" $ do
      -- GHC.Internal.Data.Bits..>>. splits into a module and an operator
      -- whose own name starts and ends with a dot.  Splitting on the last
      -- dot instead yields the module GHC.Internal.Data.Bits..>> and the
      -- name "", which is how a naive reader loses every operator.
      m <- bits
      Map.lookup (SymbolName ".>>.") m @?= only "GHC.Internal.Data.Bits"

  , testCase "the export list stops where the next section starts" $ do
      -- "direct module dependencies:" follows the exports and is full of
      -- module-shaped words.  Reading past the section turns each of them
      -- into an export that does not exist.
      m <- concurrent
      assertBool "no package-qualified word leaked in"
        (not (any (Text.isInfixOf ":" . unSymbolName) (Map.keys m)))
      Map.lookup (SymbolName "Control.Concurrent.Chan") m @?= Nothing

  , testCase "output that is not an interface dump is an error, not empty" $ do
      -- A silent empty map would read as "this module exports nothing"
      -- and delete every entry the repair pass was meant to add.
      case parseShowIface "ghc: could not find Foo.hi\n" of
        Left _   -> pure ()
        Right mo -> fail ("expected a failure, got " <> show (Map.size (moOrigins mo))
                            <> " origins")

  , testCase "an interface with no exports section is an error too" $ do
      -- The other door to the same lie: a header we understand followed by
      -- no section at all is "we could not find the exports", never "this
      -- module has none".  Distinct from a section that is present and
      -- empty, which GHC.Constants really does have.
      case parseShowIface (Text.unlines ["interface Foo 9103", "  where"]) of
        Left _   -> pure ()
        Right mo -> fail ("expected a failure, got " <> show (moOrigins mo))

  , testCase "a section that is present and empty is not an error" $ do
      case parseShowIface (Text.unlines ["interface Foo 9103", "exports:"]) of
        Left e   -> fail ("expected an empty module, got " <> show e)
        Right mo -> moOrigins mo @?= Map.empty

  , testCase "a name two modules define keeps both origins" $ do
      -- Real: ghc's own GHC.hi exports XFixitySig from both
      -- Language.Haskell.Syntax.Binds and Language.Haskell.Syntax.Extension.
      -- Keeping one is the same "first candidate wins" mistake the resolver
      -- was just cured of -- if the kept one has no indexed row and the
      -- dropped one does, the export stays unresolved for no reason.
      let dump = Text.unlines
            [ "interface GHC 9103"
            , "exports:"
            , "  Language.Haskell.Syntax.Binds.XFixitySig"
            , "  Language.Haskell.Syntax.Extension.XFixitySig"
            , "direct module dependencies: ghc-9.10.3:GHC.Cmm"
            ]
      case parseShowIface dump of
        Left e   -> fail ("parse failed: " <> show e)
        Right mo -> Map.lookup (SymbolName "XFixitySig") (moOrigins mo)
          @?= Just ( ModulePath "Language.Haskell.Syntax.Binds"
                       :| [ModulePath "Language.Haskell.Syntax.Extension"] )

  , testCase "a class exported only through its methods is not an export" $ do
      -- Real: Data.ListLike re-exports fromString without IsString, and
      -- ghc marks the unexported parent with a bar.  Taken literally it
      -- becomes an export named "IsString|", which no module has and
      -- nothing can ever resolve.  Data.String, which does export the
      -- class, prints the same line without the bar.
      let dump = Text.unlines
            [ "interface Data.ListLike 9103"
            , "exports:"
            , "  GHC.Internal.Data.String.IsString|{GHC.Internal.Data.String.fromString}"
            , "  GHC.Internal.Data.String.IsString{GHC.Internal.Data.String.fromString}"
            , "direct module dependencies: base:Data.String"
            ]
      case parseShowIface dump of
        Left e   -> fail ("parse failed: " <> show e)
        Right mo -> do
          Map.keys (moOrigins mo)
            @?= [SymbolName "IsString", SymbolName "fromString"]
          Map.lookup (SymbolName "fromString") (moOrigins mo)
            @?= only "GHC.Internal.Data.String"

  , testCase "an operator that ends in a bar is not a marker" $ do
      -- The bar only marks an unexported parent when it follows a name
      -- that could not have ended in one.  Dropping every entry with a
      -- trailing bar deleted (||) from Data.Bool and Prelude, and (<|)
      -- from base-compat, from a real index.
      let dump = Text.unlines
            [ "interface Data.Bool 9103"
            , "exports:"
            , "  GHC.Internal.Classes.||"
            , "  Data.Sequence.<|"
            , "direct module dependencies: ghc-internal:GHC.Internal.Classes"
            ]
      case parseShowIface dump of
        Left e   -> fail ("parse failed: " <> show e)
        Right mo -> do
          Map.lookup (SymbolName "||") (moOrigins mo)
            @?= only "GHC.Internal.Classes"
          Map.lookup (SymbolName "<|") (moOrigins mo) @?= only "Data.Sequence"

  , testCase "the real oracle reads a real interface off disk" $ do
      -- Everything above tests the parser against captured text, and the
      -- indexer's own tests drive a stub.  Nothing else runs the
      -- subprocess, and this repo has shipped two browsing releases that
      -- were green and broken because the IO was the part that was wrong.
      --
      -- The compiler that built this test is by construction the one whose
      -- interface files it can read, so no plan is needed to name it.  The
      -- module asked about is hypha's own, located through the same
      -- caller-supplied hook the server uses for local packages.
      mDir <- ownBuildDir
      case mDir of
        -- Reported, not silently green: an absent build tree means this
        -- case verified nothing, and saying so beats a passing tick.
        Nothing -> putStrLn
          "  (skipped: no build directory holding Hypha/Source/Origins.hi)"
        Just dir -> do
          (dbs, _) <- discoverPackageDbs ownCompiler
          built    <- mkGhcOriginOracle ownCompiler dbs (const [dir])
          case built of
            Left e  -> fail ("no oracle for the compiler that built us: " <> show e)
            Right o -> do
              r <- moduleOrigins o ownPackage (ModulePath "Hypha.Source.Origins")
              case r of
                Left e   -> fail ("real oracle failed: " <> show e)
                Right mo -> do
                  Map.lookup (SymbolName "parseShowIface") (moOrigins mo)
                    @?= only "Hypha.Source.Origins"
                  -- A type and its constructor share this name, so the
                  -- dump names the same origin twice: one origin, not two
                  -- things to try.
                  Map.lookup (SymbolName "ModuleOrigins") (moOrigins mo)
                    @?= only "Hypha.Source.Origins"
  ]

-- | The compiler that built this test suite.
ownCompiler :: CompilerId
ownCompiler = CompilerId (Text.pack ("ghc-" <> showVersion fullCompilerVersion))

-- | Any package id: the interface directory is supplied directly, so
-- nothing looks this up.
ownPackage :: PackageId
ownPackage = PackageId (PackageName "hypha") (Version "0.2.0")

-- | The build tree holding this library's own @.hi@ files, if the usual
-- @dist-newstyle@ layout is in place.
--
-- A checkout built against several compilers has one tree per compiler,
-- and only ours can be read: @cabal test all
-- --project-file=cabal.ghc-9.12.4.project@ would otherwise walk into the
-- 9.10.3 tree first and fail on @mismatched interface file versions@,
-- which is the guard working rather than the test having something to
-- say.
ownBuildDir :: IO (Maybe FilePath)
ownBuildDir = go 6 "dist-newstyle"
  where
    go :: Int -> FilePath -> IO (Maybe FilePath)
    go depth dir
      | depth < 0 = pure Nothing
      | otherwise = do
          here <- doesFileExist (dir </> "Hypha" </> "Source" </> "Origins.hi")
          if here
            then pure (Just dir)
            else do
              subs <- either (const [] . asIOException) id
                        <$> try (listDirectory dir)
              firstJustM [ go (depth - 1) (dir </> s) | s <- subs, ours s ]

    -- Every other compiler's tree, skipped by name.
    ours s = not ("ghc-" `isPrefixOf` s) || Text.pack s == unCompilerId ownCompiler

    firstJustM []       = pure Nothing
    firstJustM (a : as) = a >>= maybe (firstJustM as) (pure . Just)

    -- An unreadable directory during a best-effort walk is not the
    -- subject under test; the type annotation is all `try` needs.
    asIOException :: IOException -> ()
    asIOException _ = ()
