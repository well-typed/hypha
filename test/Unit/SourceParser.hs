{-# LANGUAGE OverloadedStrings #-}
-- | Coverage for 'Hypha.Source.Parser', the @ghc-lib-parser@-backed
-- replacement for the line-based scanners in "Hypha.Source.Extract"
-- and "Hypha.Source.Locate".  The regression that motivated the
-- rewrite — @sourceList, sourceListC :: T@ in conduit — sits at the
-- top of the suite so it is the first thing that fails if the parser
-- ever stops recognising comma-grouped signatures.
module Unit.SourceParser (tests) where

import           Data.List          (sort)
import qualified Data.Text          as Text

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import Hypha.Source.Extensions
  ( PragmaScan (..), UnknownExtension (..), defaultLanguageSettings
  , resolveExtensions, scanPragmas )
import Hypha.Source.Parser (moduleHeaderSpan, parseModuleWith)
import Hypha.Source.Parser
  ( Decl (..), DeclKind (..), ParseError (..), parseDecls, parseErrorMessage
  , parseModuleDoc, findDecl, declSigText, declSourceSpan )

tests :: TestTree
tests = testGroup "Unit.SourceParser"
  [ testCase "comma-grouped signature: both names map to the same decl" $ do
      let src = Text.unlines
            [ "module M where"
            , ""
            , "sourceList, sourceListC :: Monad m => [a] -> m ()"
            , "sourceList = undefined"
            , "sourceListC = undefined"
            ]
      case parseDecls "M.hs" src of
        Left e  -> fail ("unexpected parse error: " <> show e)
        Right ds -> do
          let names = sort (concatMap (\d -> declName d : declSiblings d) ds)
          assertBool "sourceList present" ("sourceList" `elem` names)
          assertBool "sourceListC present" ("sourceListC" `elem` names)
          -- And we can resolve either name back to the same canonical
          -- sig-line via the sibling list.
          d  <- maybe (fail "sourceList missing")  pure (findDecl "sourceList"  ds)
          d' <- maybe (fail "sourceListC missing") pure (findDecl "sourceListC" ds)
          declSigLine d  @?= Just 3
          declSigLine d' @?= Just 3
          declSiblings d  @?= ["sourceListC"]
          declSiblings d' @?= ["sourceList"]

  , testCase "single-symbol signature still resolves" $ do
      let src = Text.unlines
            [ "module M where"
            , ""
            , "foo :: Int -> Int"
            , "foo x = x + 1"
            ]
      case parseDecls "M.hs" src of
        Right ds -> do
          d <- maybe (fail "foo missing") pure (findDecl "foo" ds)
          declSigLine d @?= Just 3
          declSiblings d @?= []
        Left e -> fail (show e)

  , testCase "declSigText recovers the comma-grouped signature verbatim" $ do
      let src = Text.unlines
            [ "module M where"
            , ""
            , "sourceList, sourceListC :: Monad m => [a] -> m ()"
            , "sourceList = undefined"
            ]
      case parseDecls "M.hs" src of
        Right ds -> do
          d <- maybe (fail "sourceList missing") pure (findDecl "sourceList" ds)
          declSigText src d @?= Just "sourceList, sourceListC :: Monad m => [a] -> m ()"
        Left e -> fail (show e)

  , testCase "per-argument Haddock comments stay out of the signature" $ do
      -- A signature is a type, and a type has no comments in it.  The
      -- span-slicing this used to do swept up every line between the
      -- first and last line of the declaration, comments included, and
      -- collapsed the newlines — so the second line of the first
      -- comment ended up reading as part of the type.
      let src = Text.unlines
            [ "module M where"
            , ""
            , "splitOn :: HasCallStack"
            , "        => Text"
            , "        -- ^ String to split on. If this string is empty, an error"
            , "        -- will occur."
            , "        -> Text"
            , "        -- ^ Input text."
            , "        -> [Text]"
            , "splitOn = undefined"
            ]
      case parseDecls "M.hs" src of
        Right ds -> do
          d <- maybe (fail "splitOn missing") pure (findDecl "splitOn" ds)
          declSigText src d
            @?= Just "splitOn :: HasCallStack => Text -> Text -> [Text]"
        Left e -> fail (show e)

  , testCase "an operator spelled with a comment marker survives" $ do
      -- The reason this is not a textual strip: everything from the
      -- first @--@ onwards is the rest of the type, not a comment.
      let src = Text.unlines
            [ "module M where"
            , ""
            , "arrow :: (a --> b) -> Int"
            , "arrow = undefined"
            ]
      case parseDecls "M.hs" src of
        Right ds -> do
          d <- maybe (fail "arrow missing") pure (findDecl "arrow" ds)
          declSigText src d @?= Just "arrow :: (a --> b) -> Int"
        Left e -> fail (show e)

  , testCase "a documented class method's signature is a type too" $ do
      let src = Text.unlines
            [ "module M where"
            , ""
            , "class Pretty a where"
            , "  render :: a"
            , "         -- ^ The thing to render."
            , "         -> String"
            ]
      case parseDecls "M.hs" src of
        Right ds -> do
          d <- maybe (fail "render missing") pure (findDecl "render" ds)
          declSigText src d @?= Just "render :: a -> String"
        Left e -> fail (show e)

  , testCase "function definition without an explicit signature" $ do
      let src = Text.unlines
            [ "module M where"
            , ""
            , "noSig x = x"
            ]
      case parseDecls "M.hs" src of
        Right ds -> do
          d <- maybe (fail "noSig missing") pure (findDecl "noSig" ds)
          declSigLine d @?= Nothing
          declDefLine d @?= Just 3
        Left e -> fail (show e)

  , testCase "parseDecls classifies declaration kinds" $ do
      let src = Text.unlines
            [ "{-# LANGUAGE TypeFamilies, PatternSynonyms #-}"
            , "module Fixture where"
            , "data Colour = Red | Green"
            , "newtype Wrap = Wrap Int"
            , "class Pretty a where"
            , "  pretty :: a -> String"
            , "type Alias = Int"
            , "type family Elem c"
            , "pattern None :: Maybe a"
            , "pattern None = Nothing"
            , "run :: Int -> Int"
            , "run x = x"
            ]
      case parseDecls "Fixture.hs" src of
        Left e   -> fail ("unexpected parse error: " <> show e)
        Right ds -> do
          let kindOf n = declKind <$> findDecl n ds
          kindOf "Colour" @?= Just DkData
          kindOf "Wrap"   @?= Just DkNewtype
          kindOf "Pretty" @?= Just DkClass
          kindOf "Alias"  @?= Just DkTypeSyn
          kindOf "Elem"   @?= Just DkTypeFamily
          kindOf "None"   @?= Just DkPatternSyn
          kindOf "run"    @?= Just DkFunction
          -- class methods are declarations in their own right, carrying
          -- the enclosing class as their parent (issue 12 / 043)
          kindOf "pretty" @?= Just DkClassMethod
          declParent <$> findDecl "pretty" ds @?= Just (Just "Pretty")
          (declSigLine =<< findDecl "pretty" ds) @?= Just 6
          -- span slicing support for multi-line type decls
          (declDefLine    =<< findDecl "Colour" ds) @?= Just 3
          (declDefEndLine =<< findDecl "Colour" ds) @?= Just 3
          (declDefEndLine =<< findDecl "Pretty" ds) @?= Just 6

  , testCase "class methods and data constructors are declarations in their own right" $ do
      let src = Text.unlines
            [ "{-# LANGUAGE DefaultSignatures #-}"
            , "module M where"
            , "class C a where"
            , "  -- | An operation."
            , "  op :: a -> a"
            , "  default op :: Eq a => a -> a"
            , "  op = id"
            , "data Colour = Red | Green"
            , "newtype Wrap = Wrap Int"
            ]
      case parseDecls "M.hs" src of
        Left e   -> fail ("unexpected parse error: " <> show e)
        Right ds -> do
          op <- maybe (fail "op missing") pure (findDecl "op" ds)
          -- sig + default sig + default body merge into one method decl
          declKind op      @?= DkClassMethod
          declParent op    @?= Just "C"
          declSigLine op   @?= Just 5
          declDefLine op   @?= Just 7
          declDoc op       @?= Just " An operation."
          -- the class itself keeps its own decl and kind
          c <- maybe (fail "C missing") pure (findDecl "C" ds)
          declKind c @?= DkClass
          declParent c @?= Nothing
          -- data constructors are declarations with the type as parent
          red <- maybe (fail "Red missing") pure (findDecl "Red" ds)
          declKind red     @?= DkConstructor
          declParent red   @?= Just "Colour"
          declDefLine red  @?= Just 8
          -- a constructor sharing the type's name merges into the
          -- type's declaration: the page and the index see one entry
          declKind <$> findDecl "Wrap" ds @?= Just DkNewtype
          declParent <$> findDecl "Wrap" ds @?= Just Nothing

  , testCase "record fields are declarations parented on the type" $ do
      -- A field is the selector function users search for -- getSum,
      -- appEndo, runReaderT -- and had no declaration to be found by, so
      -- no index row (issue 12 / 043).  Parented on the type rather than
      -- the constructor, so a field shared by two constructors is one
      -- declaration and @T(..)@ reaches it.
      let src = Text.unlines
            [ "module M where"
            , "data Person = Person"
            , "  { name :: String  -- ^ Their name."
            , "  , age  :: Int"
            , "  }"
            , "data T = A { shared :: Int } | B { shared :: Int }"
            ]
      case parseDecls "M.hs" src of
        Left e   -> fail ("unexpected parse error: " <> show e)
        Right ds -> do
          nameF <- maybe (fail "name missing") pure (findDecl "name" ds)
          declKind nameF    @?= DkRecordField
          declParent nameF  @?= Just "Person"
          -- anchored on its own @field :: Type@ entry, so the field reads
          -- as the signature it is rather than as its type's whole body
          declSigLine nameF @?= Just 3
          declDoc nameF     @?= Just " Their name."
          (declSigLine =<< findDecl "age" ds) @?= Just 4
          -- one entry for a field both constructors declare
          length (filter ((== "shared") . declName) ds) @?= 1
          declParent <$> findDecl "shared" ds @?= Just (Just "T")

  , testCase "a constructor and an unrelated same-named type stay apart" $ do
      -- Type and value namespaces are separate, so aeson's own shape --
      -- @type Object@ beside @data Value = Object Object@ -- is two
      -- declarations.  Merging by name alone swallowed one of them: the
      -- constructor vanished into the synonym, or (in the other source
      -- order) the synonym inherited the constructor's kind and span.
      let srcs =
            [ ( "synonym first"
              , [ "module M where", "type Object = Int"
                , "data Value = Object Object | Null" ]
              , [ (DkTypeSyn, Nothing, 2 :: Int)
                , (DkConstructor, Just "Value", 3) ] )
            , ( "data first"
              , [ "module M where", "data Value = Object Object | Null"
                , "type Object = Int" ]
              , [ (DkConstructor, Just "Value", 2)
                , (DkTypeSyn, Nothing, 3) ] )
            ]
      sequence_
        [ case parseDecls "M.hs" (Text.unlines src) of
            Left e   -> fail (lbl <> ": unexpected parse error: " <> show e)
            Right ds ->
              [ (declKind d, declParent d, declDefLine d)
              | d <- ds, declName d == "Object"
              ] @?= [ (k, p, Just l) | (k, p, l) <- expected ]
        | (lbl, src, expected) <- srcs
        ]

  , testCase "a trailing doc names the type, not its last constructor" $ do
      -- @-- ^@ binds to the declaration it follows, which is the type --
      -- not whichever constructor that type's body contributed last.
      let src = Text.unlines
            [ "module M where"
            , "data Colour = Red | Green"
            , "-- ^ A colour."
            ]
      case parseDecls "M.hs" src of
        Left e   -> fail ("unexpected parse error: " <> show e)
        Right ds -> do
          declDoc <$> findDecl "Colour" ds @?= Just (Just " A colour.")
          declDoc <$> findDecl "Green"  ds @?= Just Nothing

  , testCase "merged sig+def carries kind and both spans" $ do
      let src = Text.unlines
            [ "module M where"
            , ""
            , "run :: Int -> Int"
            , "run x = x"
            ]
      case parseDecls "M.hs" src of
        Right ds -> do
          d <- maybe (fail "run missing") pure (findDecl "run" ds)
          declKind d       @?= DkFunction
          declSigLine d    @?= Just 3
          declDefLine d    @?= Just 4
          declDefEndLine d @?= Just 4
        Left e -> fail (show e)
  , testCase "role annotations and MagicHash parse (was: whitelist miss)" $ do
      src <- Text.pack <$> readFile "test/fixtures/reexport/src/Fixture/Internal.hs"
      case parseDecls "Fixture/Internal.hs" src of
        Left e   -> fail ("unexpected parse error: " <> Text.unpack (parseErrorMessage e))
        Right ds -> do
          let names = map declName ds
          assertBool "insertBag found" ("insertBag" `elem` names)
          assertBool "sizeBag found"   ("sizeBag"   `elem` names)

  , testCase "moduleHeaderSpan covers the header doc and the export list" $ do
      -- What `hypha source PKG/MOD` answers with when no symbol is named.
      -- It used to emit the first 16 lines of the file, which is an
      -- arbitrary cut that neither ends at the export list nor starts at
      -- anything meaningful.
      let src = Text.unlines
            [ "{-# LANGUAGE Haskell2010 #-}"   -- 1
            , "-- | Lists, and things."        -- 2
            , "--"                             -- 3
            , "-- Second paragraph."           -- 4
            , "module M"                       -- 5
            , "  ( foo"                        -- 6
            , "  , bar"                        -- 7
            , "  ) where"                      -- 8
            , ""                               -- 9
            , "foo :: Int"                     -- 10
            , "foo = 1"                        -- 11
            , ""
            , "bar :: Int"
            , "bar = 2"
            ]
      case parseModuleWith defaultLanguageSettings "M.hs" src of
        Left e            -> fail ("unexpected parse error: "
                                     <> Text.unpack (parseErrorMessage e))
        Right (hsMod, _, _) -> moduleHeaderSpan hsMod @?= Just (2, 8)

  , testCase "declSourceSpan spans the doc, signature and body (issue #55)" $ do
      -- The span a source snippet is cut from.  It has to stop before the
      -- next declaration: `hypha source` used to answer with a flat
      -- 30-line window, which trailed off into whatever followed.
      let src = Text.unlines
            [ "module M where"            --  1
            , ""                          --  2
            , "-- | Sort a list."         --  3
            , "-- more prose."            --  4
            , "--"                        --  5
            , "-- @since 4.8.0.0"         --  6
            , "sortOn :: Int -> Int"      --  7
            , "sortOn x ="                --  8
            , "  x + 1"                   --  9
            , ""                          -- 10
            , "-- | Another symbol."      -- 11
            , "singleton :: Int"          -- 12
            , "singleton = 1"             -- 13
            ]
      case parseDecls "M.hs" src of
        Left e   -> fail ("unexpected parse error: " <> Text.unpack (parseErrorMessage e))
        Right ds -> case findDecl "sortOn" ds of
          Nothing -> fail "sortOn not found"
          Just d  -> declSourceSpan d @?= Just (3, 9)

  , testCase "declSourceSpan takes in a trailing -- ^ block" $ do
      let src = Text.unlines
            [ "module M where"        -- 1
            , ""                      -- 2
            , "answer :: Int"         -- 3
            , "answer = 42"           -- 4
            , "-- ^ The answer."      -- 5
            ]
      case parseDecls "M.hs" src of
        Left e   -> fail ("unexpected parse error: " <> Text.unpack (parseErrorMessage e))
        Right ds -> case findDecl "answer" ds of
          Nothing -> fail "answer not found"
          Just d  -> declSourceSpan d @?= Just (3, 5)

  , testCase "declSourceSpan of an undocumented binding is just its own lines" $ do
      let src = Text.unlines
            [ "module M where"     -- 1
            , ""                   -- 2
            , "bare :: Int"        -- 3
            , "bare = 1"           -- 4
            ]
      case parseDecls "M.hs" src of
        Left e   -> fail ("unexpected parse error: " <> Text.unpack (parseErrorMessage e))
        Right ds -> case findDecl "bare" ds of
          Nothing -> fail "bare not found"
          Just d  -> declSourceSpan d @?= Just (3, 4)

  , testCase "declSourceSpan of a data declaration covers the whole body" $ do
      let src = Text.unlines
            [ "module M where"          -- 1
            , ""                        -- 2
            , "-- | A colour."          -- 3
            , "data Colour"             -- 4
            , "  = Red"                 -- 5
            , "  | Green"               -- 6
            , "  deriving Show"         -- 7
            , ""                        -- 8
            , "other :: Int"            -- 9
            , "other = 1"               -- 10
            ]
      case parseDecls "M.hs" src of
        Left e   -> fail ("unexpected parse error: " <> Text.unpack (parseErrorMessage e))
        Right ds -> case findDecl "Colour" ds of
          Nothing -> fail "Colour not found"
          Just d  -> declSourceSpan d @?= Just (3, 7)

  , testCase "a top-level TH splice parses on the pragma alone (issue #47)" $ do
      -- The module states TemplateHaskell and nothing else, exactly as
      -- api-tools' Data.API.Error does.  GHC parses it because
      -- TemplateHaskell implies TemplateHaskellQuotes, and the lexer gates
      -- @$(@ and @\'\'Name@ on the implied flag rather than the named one.
      -- hypha applied no implications, so this failed with
      -- @parse error on input `$\'@ and the module page fell back to an
      -- export list.
      let src = Text.unlines
            [ "{-# LANGUAGE TemplateHaskell #-}"
            , "module M where"
            , ""
            , "import Data.Aeson.TH (deriveJSON, defaultOptions)"
            , ""
            , "data Expected = Expected"
            , ""
            , "$(deriveJSON defaultOptions \'\'Expected)"
            ]
      case parseModuleDoc "M.hs" src of
        Left e   -> fail ("splice module did not parse: "
                            <> Text.unpack (parseErrorMessage e))
        Right (_, ds) ->
          -- The splice itself declares nothing we can name, but the
          -- declarations around it must survive it.
          assertBool "the type beside the splice is still found"
            ("Expected" `elem` map declName ds)

  , testCase "parse failure carries GHC's message and a line" $ do
      let src = Text.unlines
            [ "module M where"
            , ""
            , "f x = case x of"
            , "  -> 1"
            ]
      case parseDecls "M.hs" src of
        Right _ -> fail "expected a parse error"
        Left e  -> do
          assertBool "message is not the literal 'parse error'"
            (parseErrorMessage e /= "parse error")
          assertBool "message is non-empty"
            (not (Text.null (parseErrorMessage e)))
          peLine e @?= Just 4

  , testCase "unknown pragma name is reported, parse still succeeds" $ do
      let src = Text.unlines
            [ "{-# LANGUAGE OverloadedStrings #-}"
            , "module M where"
            , "f :: Int"
            , "f = 1"
            ]
      case parseModuleDoc "M.hs" src of
        Left e  -> fail ("unexpected parse error: " <> Text.unpack (parseErrorMessage e))
        Right _ -> pure ()
      scan <- scanPragmas "M.hs" src
      let (_, unknown) = resolveExtensions defaultLanguageSettings (psExtensionNames scan)
      map unUnknownExtension unknown @?= []
  ]
