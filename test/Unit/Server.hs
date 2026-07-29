{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Unit.Server (tests) where

import Control.Exception (SomeException, evaluate, try)
import Data.IP (IP (..), toIPv4, toIPv6)
import Data.Text qualified as Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)
import Text.RawString.QQ (r)

import Data.Text.Lazy qualified as LText
import Lucid (renderText)

import Hypha.Command.Server
  ( BindAddr (..), BindError (..), briefException, parseBind )
import Hypha.Search.Collapse (collapseRows)
import Hypha.Search.Index (DefinitionRef (..))
import Hypha.Source.Extract
  ( DocEntry (..), EntryOrigin (..), ModuleDocInfo (..) )
import Hypha.Types.ComponentName (ComponentKey (..))
import Hypha.Search.Fuzzy (mkSymbolRow)
import Hypha.Search.Index (Visibility (..))
import Hypha.Search.Reexport (DefinitionSite (..))
import Hypha.Server.ModuleDoc
  ( ModuleDocView (..), SourceDoc (..), SymbolCardData (..) )
import Hypha.Server.Ui.Doc (symbolCard)
import Hypha.Source.Locate (Provenance (..))
import Hypha.Source.Parser (DeclKind (..))
import Hypha.Types.PackageId (PackageName (..), Version (..))
import Hypha.Types.SymbolPath (ModulePath (..))
import Util.Row (rowIn)
import Hypha.Server.App (mimeFor, sanitizeSegments)
import Hypha.Server.Ui.ModuleDoc (modulePage)
import Hypha.Server.Ui.Search (highlightTokens, resultsFragment)
import Hypha.Server.Ui.Tree (hackageLink, splitByOrigin)
import Hypha.Types.BuildPlan (PackageOrigin (..))

mkIPv4 :: [Int] -> IP
mkIPv4 = IPv4 . toIPv4

mkIPv6 :: [Int] -> IP
mkIPv6 = IPv6 . toIPv6

tests :: TestTree
tests = testGroup "Unit.Server"
  [ testGroup "Server.parseBind" parseBindTests
  , testGroup "Server.briefException" briefExceptionTests
  , testGroup "App.sanitizeSegments" sanitizeSegmentsTests
  , testGroup "App.mimeFor" mimeForTests
  , testGroup "Tree.splitByOrigin" splitByOriginTests
  , testGroup "Tree.hackageLink" hackageLinkTests
  , testGroup "Doc.symbolCard" symbolCardTests
  , testGroup "Search.highlightTokens" highlightTokensTests
  , testGroup "Search.resultsFragment" resultsFragmentTests
  , testGroup "ModuleDoc.modulePage" modulePageTests
  ]

sanitizeSegmentsTests :: [TestTree]
sanitizeSegmentsTests =
  [ testCase "plain html file accepted" $
      sanitizeSegments ["Data-Map.html"] @?= Just ["Data-Map.html"]
  , testCase "nested src path accepted" $
      sanitizeSegments ["src", "Foo.html"] @?= Just ["src", "Foo.html"]
  , testCase "empty path rejected" $
      sanitizeSegments [] @?= Nothing
  , testCase "parent traversal rejected" $
      sanitizeSegments ["..", "x"] @?= Nothing
  , testCase "slash inside a segment rejected" $
      sanitizeSegments ["a/b"] @?= Nothing
  , testCase "empty segment rejected" $
      sanitizeSegments [""] @?= Nothing
  , testCase "dotfile rejected" $
      sanitizeSegments [".hidden"] @?= Nothing
  ]

mimeForTests :: [TestTree]
mimeForTests =
  [ testCase "css"       $ mimeFor "ocean.css"  @?= "text/css; charset=utf-8"
  , testCase "min.js"    $ mimeFor "b.min.js"   @?= "application/javascript; charset=utf-8"
  , testCase "html"      $ mimeFor "c.html"     @?= "text/html; charset=utf-8"
  , testCase "png"       $ mimeFor "d.png"      @?= "image/png"
  , testCase "extension-less falls back to octet-stream" $
      mimeFor "LICENSE" @?= "application/octet-stream"
  ]

splitByOriginTests :: [TestTree]
splitByOriginTests =
  [ testCase "local packages land in the project group, rest are dependencies" $ do
      let local   = ("mine",  OriginLocal "/src/mine")
          hackage = ("aeson", OriginHackage)
          dist    = ("base",  OriginDistribution)
          srp     = ("dep",   OriginSourceRepo Nothing Nothing Nothing)
      splitByOrigin [hackage, local, dist, srp]
        @?= ([local], [hackage, dist, srp])
  ]

hackageLinkTests :: [TestTree]
hackageLinkTests =
  [ testCase "Hackage origin links to the pinned version" $
      renderLink "aeson" "2.2.1.0" OriginHackage
        `shouldContain` "https://hackage.haskell.org/package/aeson-2.2.1.0"

  , testCase "distribution (boot) packages link too" $
      -- containers, base and every other boot library IS published on
      -- Hackage; withholding the link was the bug.
      renderLink "containers" "0.7" OriginDistribution
        `shouldContain` "https://hackage.haskell.org/package/containers-0.7"

  , testCase "local packages get no link" $
      renderLink "myapp" "0.1.0" (OriginLocal "/src/myapp") @?= ""

  , testCase "source-repository-package gets no link" $
      renderLink "forked" "1.0" (OriginSourceRepo Nothing Nothing Nothing) @?= ""

  , testCase "local tarball gets no link" $
      renderLink "tar" "1.0" (OriginLocalTarball "/t.tar.gz") @?= ""

  , testCase "remote tarball gets no link" $
      renderLink "tar" "1.0" (OriginRemoteTarball "https://x/t.tar.gz") @?= ""
  ]
  where
    renderLink pkg ver origin = LText.toStrict
      (renderText (hackageLink (PackageName pkg) (Version ver) origin))

    shouldContain hay needle =
      assertBool (show needle <> " not in " <> show hay) (needle `Text.isInfixOf` hay)


highlightTokensTests :: [TestTree]
highlightTokensTests =
  [ testCase "single token wraps its first occurrence" $
      renderHl ["map"] "fmap" @?= "f<mark>map</mark>"
  , testCase "match is case-insensitive but preserves original text" $
      renderHl ["MAP"] "mapMaybe" @?= "<mark>map</mark>Maybe"
  , testCase "several tokens highlight without overlapping" $
      renderHl ["fold", "map"] "foldMap" @?= "<mark>fold</mark><mark>Map</mark>"
  , testCase "no match passes text through verbatim" $
      renderHl ["zip"] "fold" @?= "fold"
  , testCase "overlapping tokens never nest marks" $
      renderHl ["foldm", "map"] "foldmap" @?= "<mark>foldm</mark>ap"
  ]
  where
    renderHl toks t = LText.toStrict (renderText (highlightTokens toks t))

-- | What the reader actually sees behind a @+N@ badge.
--
-- The collapse model was tested and the renderer was not, which is how a
-- flagship case shipped listing the defining module twice: the definition
-- is usually a presentation as well, so it was in 'srAlternates' /and/ in
-- a prepended "defines it" row.
-- | What a module page /claims/, which is a different question from what
-- the extractor returned.
modulePageTests :: [TestTree]
modulePageTests =
  [ testCase "an entry we could not place makes no claim and no link" $ do
      -- The live server said "from base:GHC.Internal.Control.Monad" for
      -- Bool, True, Just and map on base/Prelude -- one guess, presented
      -- as fact and linked, for every name it failed to resolve.
      let html = renderPage (entry (EntryUnplaced (ModulePath "GHC.Internal.Control.Monad")))
      assertBool "hedges instead of naming a definition site"
        ("re-exported, origin unresolved" `Text.isInfixOf` html)
      assertBool "does not present the guess as the origin"
        (not ("from GHC.Internal.Control.Monad" `Text.isInfixOf` html))
      assertBool "and does not link anywhere for it"
        (not ("/pkg/base/GHC.Internal.Control.Monad" `Text.isInfixOf` html))

  , testCase "a resolved re-export still names its definition site" $ do
      let def  = DefinitionRef (ComponentKey "ghc-internal")
                               (ModulePath "GHC.Internal.Data.Traversable")
          html = renderPage (entry (EntryReexport def))
      assertBool "names the defining component and module"
        ("from ghc-internal:GHC.Internal.Data.Traversable" `Text.isInfixOf` html)
      assertBool "and links there"
        ("/pkg/ghc-internal/GHC.Internal.Data.Traversable" `Text.isInfixOf` html)
  ]
  where
    entry origin = DocEntry
      { deName      = "mapAccumL"
      , deKind      = DkFunction
      , deSignature = Nothing
      , deHaddock   = Nothing
      , deSigLine   = Nothing
      , deDefLine   = Nothing
      , deOrigin    = origin
      }

    renderPage e = LText.toStrict . renderText $
      modulePage "base" "Prelude"
        (ViewFromSource (SourceDoc (ModuleDocInfo Nothing [e] []) Nothing))

resultsFragmentTests :: [TestTree]
resultsFragmentTests =
  [ testCase "the defining module is listed once, tagged, not repeated" $ do
      -- The whole group: Data.Map.Strict presents insertWith and wins,
      -- Data.Map.Strict.Internal both defines and exposes it.  So the
      -- definition is in srAlternates, and a separate "defines it" row
      -- listed it twice behind a badge that said "+1".
      let opened = altList (renderResults ["insertwith"]
            [ mapRow "Data.Map.Strict"          "Data.Map.Strict.Internal"
            , mapRow "Data.Map.Strict.Internal" "Data.Map.Strict.Internal"
            ])
      countOf "containers:Data.Map.Strict.Internal" opened @?= 1
      countOf "alt-tag" opened @?= 1
      countOf "<li>" opened @?= 1

  , testCase "the badge counts the rows it opens" $ do
      let html = renderResults ["insertwith"]
            [ mapRow "Data.Map.Strict"          "Data.Map.Strict.Internal"
            , mapRow "Data.Map.Strict.Internal" "Data.Map.Strict.Internal"
            ]
      assertBool ("expected +1 in " <> show html) ("+1" `Text.isInfixOf` html)

  , testCase "a definition that is no presentation is still named" $ do
      -- base's Data.List and Data.Traversable both expose mapAccumL; the
      -- module that defines it exposes nothing the search indexed, so the
      -- disclosure has to append it rather than find it among the folded-in
      -- presentations.
      let opened = altList (renderResults ["mapaccuml"]
            [ rowIn "base" "Data.Traversable" "mapAccumL" "sig"
                    "GHC.Internal.Data.Traversable" Exposed
            , rowIn "base" "Data.List" "mapAccumL" "sig"
                    "GHC.Internal.Data.Traversable" Exposed
            ])
      countOf "base:Data.Traversable" opened             @?= 1
      countOf "base:GHC.Internal.Data.Traversable" opened @?= 1
      countOf "alt-tag" opened                           @?= 1
      countOf "<li>" opened                              @?= 2

  , testCase "a result with nothing folded in gets no disclosure" $ do
      let html = renderResults ["insertwith"]
                   [ mapRow "Data.Map.Strict" "Data.Map.Strict" ]
      countOf "alt-group" html @?= 0
  ]
  where
    mapRow presented defined =
      rowIn "containers" presented "insertWith"
        "insertWith :: Ord k => k -> a -> Map k a -> Map k a" defined Exposed

    renderResults tokens =
      LText.toStrict . renderText . resultsFragment tokens
        . collapseRows . map mkSymbolRow

    -- Only what the disclosure opens: the badge's tooltip names the same
    -- modules, and counting both would not say which side listed one twice.
    altList = snd . Text.breakOn "<ul class=\"alt-list\">"

    countOf needle = length . Text.breakOnAll needle

parseBindTests :: [TestTree]
parseBindTests =
  [ testCase "localhost:4287 accepted" $
      parseBind "localhost:4287" @?= Right (BindAddr (mkIPv4 [127,0,0,1]) 4287)

  , testCase "127.0.0.1:4287 accepted" $
      parseBind "127.0.0.1:4287" @?= Right (BindAddr (mkIPv4 [127,0,0,1]) 4287)

  , testCase "127.0.0.5:4287 accepted (within 127.0.0.0/8)" $
      parseBind "127.0.0.5:4287" @?= Right (BindAddr (mkIPv4 [127,0,0,5]) 4287)

  , testCase "[::1]:4287 accepted" $
      parseBind "[::1]:4287" @?= Right (BindAddr (mkIPv6 [0,0,0,0,0,0,0,1]) 4287)

  , testCase "0.0.0.0:4287 refused" $
      parseBind "0.0.0.0:4287" @?= Left (BindNonLoopback "0.0.0.0:4287")

  , testCase "public IP refused" $
      parseBind "192.168.1.10:4287" @?= Left (BindNonLoopback "192.168.1.10:4287")

  , testCase "[2001:db8::1]:4287 refused (non-loopback IPv6)" $
      parseBind "[2001:db8::1]:4287" @?= Left (BindNonLoopback "[2001:db8::1]:4287")

  , testCase "malformed input refused" $
      parseBind "not-a-bind" @?= Left (BindMalformed "not-a-bind")

  , testCase "non-numeric port refused" $
      parseBind "127.0.0.1:zzz" @?= Left (BindMalformed "127.0.0.1:zzz")

  , testCase "out-of-range port refused" $
      parseBind "127.0.0.1:70000" @?= Left (BindMalformed "127.0.0.1:70000")

  , testCase "bare IPv6 without brackets rejected (ambiguous)" $
      parseBind "::1:4287" @?= Left (BindMalformed "::1:4287")
  ]

briefExceptionTests :: [TestTree]
briefExceptionTests =
  [ testCase "ErrorCall message preserved exactly, CallStack stripped" $ do
      -- GHC (>= 9.10) attaches a CallStack to every 'error' call in two
      -- places: as a legacy location string inside ErrorCall, and as a
      -- Backtraces annotation in SomeException's ExceptionContext.
      -- The ErrorCall pattern synonym discards the location; the
      -- SomeException pattern match drops the context.  What remains is
      -- exactly the message string passed to 'error'.
      Left (e :: SomeException) <- try (evaluate (error "boom" :: ()))
      briefException e @?= "boom"

  , testCase "error message containing the CallStack header is not truncated" $ do
      -- Regression: the old string-matching approach searched for
      -- "CallStack (from HasCallStack)" in the rendered output and
      -- truncated there.  An error message that /itself/ starts with
      -- that string would be truncated to empty.  The structured
      -- approach extracts the message via the ErrorCall pattern synonym,
      -- which is immune to this class of bug.
      Left (e :: SomeException) <-
        try (evaluate (error [r|CallStack (from HasCallStack): oops|] :: ()))
      briefException e @?= "CallStack (from HasCallStack): oops"

  , testCase "multi-line ErrorCall message is collapsed to one line" $ do
      Left (e :: SomeException) <-
        try (evaluate (error [r|line1
line2
line3|] :: ()))
      briefException e @?= "line1 line2 line3"

  , testCase "non-ErrorCall exception message is preserved" $ do
      result <- try (evaluate (1 `div` 0 :: Int)) :: IO (Either SomeException Int)
      case result of
        Left e  -> briefException e @?= "divide by zero"
        Right _ -> assertFailure "division by zero must throw"
  ]

symbolCardTests :: [TestTree]
symbolCardTests =
  [ testCase "a re-exported symbol names both modules" $ do
      let html = renderCard SymbolCardData
            { scdSignature  = Just "insertWith :: Ord k => k -> a"
            , scdHaddock    = Nothing
            , scdModule     = "Data.Map.Strict.Internal"
            , scdComponent  = "containers"
            , scdRequested  = "Data.Map.Strict"
            , scdProvenance = Resolved (DefinedIn (ModulePath "Data.Map.Strict.Internal"))
            , scdLine       = Just 552
            , scdKind       = Just DkFunction
            }
      assertBool "mentions the presentation module"
        ("Data.Map.Strict" `Text.isInfixOf` html)
      assertBool "mentions the definition module"
        ("Data.Map.Strict.Internal" `Text.isInfixOf` html)
      assertBool "says it is a re-export"
        ("Re-exported by" `Text.isInfixOf` html)

  , testCase "a locally defined symbol does not claim a re-export" $ do
      let html = renderCard SymbolCardData
            { scdSignature  = Just "insertWith :: Ord k => k -> a"
            , scdHaddock    = Nothing
            , scdModule     = "Data.Map.Internal"
            , scdComponent  = "containers"
            , scdRequested  = "Data.Map.Internal"
            , scdProvenance = Resolved DefinedHere
            , scdLine       = Just 552
            , scdKind       = Just DkFunction
            }
      assertBool "no re-export line"
        (not ("Re-exported by" `Text.isInfixOf` html))

  , testCase "a missing signature says so instead of rendering blank" $ do
      let html = renderCard SymbolCardData
            { scdSignature  = Nothing
            , scdHaddock    = Nothing
            , scdModule     = "Data.Map.Internal"
            , scdComponent  = "containers"
            , scdRequested  = "Data.Map.Internal"
            , scdProvenance = Resolved DefinedHere
            , scdLine       = Nothing
            , scdKind       = Nothing
            }
      assertBool "explains the absence"
        ("no signature" `Text.isInfixOf` Text.toLower html)

  , testCase "a swept location is labelled as a guess" $ do
      -- Before this, a swept location rendered identically to a resolved
      -- one -- which is how Data/Set/Internal.hs came to look like fact.
      let html = renderCard SymbolCardData
            { scdSignature  = Just "balanceL :: a"
            , scdHaddock    = Nothing
            , scdModule     = "Data.Set.Internal"
            , scdComponent  = "containers"
            , scdRequested  = "Data.Map.Internal"
            , scdProvenance = GuessedBySweep "package cabal could not be parsed"
            , scdLine       = Just 1746
            , scdKind       = Just DkFunction
            }
      assertBool "surfaces the uncertainty"
        ("best guess" `Text.isInfixOf` Text.toLower html)

  , testCase "a cross-package definition names and links the owning package" $ do
      -- base's Data.Traversable documents mapAccumL; ghc-internal declares
      -- it.  Every link to the definition has to leave base, or it points
      -- at a module base does not have.
      let html = renderCard SymbolCardData
            { scdSignature  = Just "mapAccumL :: a"
            , scdHaddock    = Nothing
            , scdModule     = "GHC.Internal.Data.Traversable"
            , scdComponent  = "ghc-internal"
            , scdRequested  = "Data.Traversable"
            , scdProvenance =
                Resolved (DefinedOutside (ModulePath "GHC.Internal.Data.Traversable"))
            , scdLine       = Just 120
            , scdKind       = Just DkFunction
            }
      assertBool "names the defining package"
        ("ghc-internal:GHC.Internal.Data.Traversable" `Text.isInfixOf` html)
      assertBool "module link leaves the asking package"
        ("/pkg/ghc-internal/GHC.Internal.Data.Traversable" `Text.isInfixOf` html)
      assertBool "source link leaves the asking package"
        ("/source/ghc-internal/GHC.Internal.Data.Traversable" `Text.isInfixOf` html)
      assertBool "never links the definition under the asking package"
        (not ("/pkg/containers/GHC.Internal" `Text.isInfixOf` html))
  ]
  where
    -- The page is reached as containers/… in every case above; the
    -- cross-package case deliberately disagrees with it.
    renderCard = LText.toStrict . renderText . symbolCard "insertWith" "containers"
