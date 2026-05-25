{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
module Property.OutputJson (tests) where

import Data.Aeson (object, (.=))
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text (Text)

import Test.Falsify.Generator qualified as Gen
import Test.Falsify.Predicate qualified as P
import Test.Falsify.Property (gen, assert)
import Test.Falsify.Range qualified as Range
import Test.Tasty.Falsify (testProperty)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty (TestTree, testGroup)

import Hypha.Cli.Types
import Hypha.Error (HyphaError (..), NotFoundReason (..))
import Hypha.Types.PackageId (PackageName (..))
import Hypha.Output.Json (ToOutcomeJson (..), encodeEnvelope, filterSelect, objectKeys)
import Hypha.Output.Outcome (successOutcome)

-------------------------------------------------------------------------------
-- Sample command result: SymbolCard

data SourceLoc = SourceLoc
  { slPath   :: !Text
  , slLine   :: !Int
  , slColumn :: !(Maybe Int)
  }
  deriving stock (Show)

data SymbolCard = SymbolCard
  { scName      :: !Text
  , scKind      :: !Text
  , scPackage   :: !Text
  , scVersion   :: !Text
  , scModule    :: !Text
  , scSignature :: !(Maybe Text)
  , scHaddock   :: !(Maybe Text)
  , scSource    :: !(Maybe SourceLoc)
  }
  deriving stock (Show)

instance ToOutcomeJson SymbolCard where
  toCompactJSON card = object
    [ "name"    .= scName card
    , "kind"    .= scKind card
    , "package" .= scPackage card
    , "version" .= scVersion card
    , "module"  .= scModule card
    ]

  toFullJSON card = object
    [ "name"        .= scName card
    , "kind"        .= scKind card
    , "package"     .= scPackage card
    , "version"     .= scVersion card
    , "module"      .= scModule card
    , "signature"   .= scSignature card
    , "haddock_raw" .= scHaddock card
    , "source"      .= fmap toSourceObj (scSource card)
    ]
    where
      toSourceObj s = object
        [ "path"   .= slPath s
        , "line"   .= slLine s
        , "column" .= slColumn s
        ]

-------------------------------------------------------------------------------
-- Generators

genText :: Gen.Gen Text
genText = Text.pack <$> Gen.list (Range.between (1, 12)) genChar
  where
    genChar = Gen.elem (pure 'a' <> pure 'B' <> pure '2' <> pure '_')

genMaybe :: Gen.Gen a -> Gen.Gen (Maybe a)
genMaybe g = do
  b <- Gen.bool False
  if b then Just <$> g else pure Nothing

genSourceLoc :: Gen.Gen SourceLoc
genSourceLoc = SourceLoc
  <$> genText
  <*> Gen.int (Range.between (1, 9999))
  <*> genMaybe (Gen.int (Range.between (1, 200)))

genSymbolCard :: Gen.Gen SymbolCard
genSymbolCard = SymbolCard
  <$> genText
  <*> genText
  <*> genText
  <*> genText
  <*> genText
  <*> genMaybe genText
  <*> genMaybe genText
  <*> genMaybe genSourceLoc

-------------------------------------------------------------------------------
-- Property

tests :: TestTree
tests = testGroup "OutputJson"
  [ testProperty "compact key set is subset of full key set (SymbolCard)" $ do
      card <- gen genSymbolCard
      let compact = objectKeys (toCompactJSON card)
          full    = objectKeys (toFullJSON card)
      assert $ P.eq P..$ ("compact ⊆ full", True) P..$ ("got", compact `Set.isSubsetOf` full)
  , testCase "encodeEnvelope produces valid success envelope"           testSuccessEnvelope
  , testCase "encodeEnvelope produces valid failure envelope"           testFailureEnvelope
  , testCase "filterSelect keeps only listed top-level keys"            testFilterSelect
  , testCase "filterSelect empty list is identity"                      testFilterSelectNoop
  ]

testSuccessEnvelope :: IO ()
testSuccessEnvelope = do
  let outcome = successOutcome (toCompactJSON (SymbolCard "x" "fn" "p" "1.0" "M" Nothing Nothing Nothing))
      val     = encodeEnvelope SymbolCmd (Right outcome)
      keys    = objectKeys val
  keys @?= Set.fromList
    [ "schema", "command", "ok", "outside_plan", "overrides", "result", "actions", "related" ]

testFailureEnvelope :: IO ()
testFailureEnvelope = do
  let err  = NotFound (NotFoundPackageInPlan (PackageName "missing"))
      val  = encodeEnvelope SymbolCmd (Left err)
      keys = objectKeys val
  keys @?= Set.fromList [ "schema", "command", "ok", "error", "actions" ]

testFilterSelect :: IO ()
testFilterSelect = do
  let outcome = successOutcome (toCompactJSON (SymbolCard "x" "fn" "p" "1.0" "M" Nothing Nothing Nothing))
      env     = encodeEnvelope SymbolCmd (Right outcome)
      filtered = filterSelect ["schema", "ok", "result"] env
      keys    = objectKeys filtered
  keys @?= Set.fromList [ "schema", "ok", "result" ]

testFilterSelectNoop :: IO ()
testFilterSelectNoop = do
  let outcome = successOutcome (toCompactJSON (SymbolCard "x" "fn" "p" "1.0" "M" Nothing Nothing Nothing))
      env     = encodeEnvelope SymbolCmd (Right outcome)
      filtered = filterSelect [] env
  filtered @?= env
