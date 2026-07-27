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
import Hypha.Search.Collapse (SearchResult (..), SymbolResult (..))
import Hypha.Search.Reexport (DefinitionSite (..))
import Hypha.Server.ModuleDoc (SymbolCardData (..))
import Hypha.Server.Ui.Doc (symbolCard)
import Hypha.Source.Locate (Provenance (..))
import Hypha.Source.Parser (DeclKind (..))
import Hypha.Types.ComponentName (ComponentKey (..))
import Hypha.Types.PackageId (PackageName (..), Version (..))
import Hypha.Types.SymbolPath (ModulePath (..), Signature (..), SymbolName (..))
import Hypha.Server.App (mimeFor, sanitizeSegments, scopeSearchRows)
import Hypha.Server.Ui.Search (highlightTokens)
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
  , testGroup "App.scopeSearchRows" scopeSearchRowsTests
  , testGroup "Tree.splitByOrigin" splitByOriginTests
  , testGroup "Tree.hackageLink" hackageLinkTests
  , testGroup "Doc.symbolCard" symbolCardTests
  , testGroup "Search.highlightTokens" highlightTokensTests
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

scopeSearchRowsTests :: [TestTree]
scopeSearchRowsTests =
  [ testCase "no scope parameter keeps every row" $
      scopeSearchRows Nothing rows @?= rows
  , testCase "empty scope parameter keeps every row" $
      scopeSearchRows (Just "") rows @?= rows
  , testCase "non-empty scope keeps only matching rows" $
      scopeSearchRows (Just "aeson") rows @?= [aesonRow]
  , testCase "scope matching no package yields no rows" $
      scopeSearchRows (Just "nope") rows @?= []
  ]
  where
    aesonRow = ResultSymbol SymbolResult
      { srComponent  = ComponentKey "aeson"
      , srModule     = ModulePath "Data.Aeson"
      , srName       = SymbolName "encode"
      , srSignature  = Signature "Value -> ByteString"
      , srDefModule  = ModulePath "Data.Aeson.Encoding"
      , srAlternates = 0
      }
    containersRow = ResultSymbol SymbolResult
      { srComponent  = ComponentKey "containers"
      , srModule     = ModulePath "Data.Map"
      , srName       = SymbolName "lookup"
      , srSignature  = Signature "k -> Map k v -> Maybe v"
      , srDefModule  = ModulePath "Data.Map.Internal"
      , srAlternates = 0
      }
    rows = [aesonRow, containersRow]

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
            , scdRequested  = "Data.Map.Internal"
            , scdProvenance = GuessedBySweep "package cabal could not be parsed"
            , scdLine       = Just 1746
            , scdKind       = Just DkFunction
            }
      assertBool "surfaces the uncertainty"
        ("best guess" `Text.isInfixOf` Text.toLower html)
  ]
  where
    renderCard = LText.toStrict . renderText . symbolCard "insertWith" "containers"
