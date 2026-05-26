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

import Hypha.Source.Parser
  ( Decl (..), parseDecls, findDecl, declSigText )

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
  ]
