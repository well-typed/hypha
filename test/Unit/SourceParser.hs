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
import Hypha.Source.Parser
  ( Decl (..), DeclKind (..), ParseError (..), parseDecls, parseErrorMessage
  , parseModuleDoc, findDecl, declSigText )

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
