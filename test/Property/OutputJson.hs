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
import Hypha.Output.Json
  ( ToOutcomeJson (..), encodeSuccessEnvelope, encodeErrorEnvelope
  , filterSelect, objectKeys, parseSelectList
  , EnvelopeOpts (..), defaultEnvelopeOpts, unmatchedSelect )
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
  , testCase "encodeSuccessEnvelope produces valid success envelope"    testSuccessEnvelope
  , testCase "encodeErrorEnvelope produces valid failure envelope"      testFailureEnvelope
  , testCase "filterSelect keeps only listed top-level keys"            testFilterSelect
  , testCase "filterSelect empty list is identity"                      testFilterSelectNoop
  , testCase "select aliases canonicalize to the wire field names"      testSelectAliases
  , testCase "aliased select keeps the aliased fields"                  testSelectAliasesProject
  , testCase "a select name the command cannot answer is reported"      testUnmatchedSelect
  ]

testSuccessEnvelope :: IO ()
testSuccessEnvelope = do
  let outcome = successOutcome SymbolCmd (toCompactJSON (SymbolCard "x" "fn" "p" "1.0" "M" Nothing Nothing Nothing))
      val     = encodeSuccessEnvelope outcome
      keys    = objectKeys val
  keys @?= Set.fromList
    [ "result" ]

testFailureEnvelope :: IO ()
testFailureEnvelope = do
  let err  = NotFound (NotFoundPackageInPlan (PackageName "missing"))
      val  = encodeErrorEnvelope err
      keys = objectKeys val
  keys @?= Set.fromList [ "error" ]

testFilterSelect :: IO ()
testFilterSelect = do
  let outcome = successOutcome SymbolCmd (toCompactJSON (SymbolCard "x" "fn" "p" "1.0" "M" Nothing Nothing Nothing))
      env     = encodeSuccessEnvelope outcome
      filtered = filterSelect ["result"] env
      keys    = objectKeys filtered
  keys @?= Set.fromList [ "result" ]

testFilterSelectNoop :: IO ()
testFilterSelectNoop = do
  let outcome = successOutcome SymbolCmd (toCompactJSON (SymbolCard "x" "fn" "p" "1.0" "M" Nothing Nothing Nothing))
      env     = encodeSuccessEnvelope outcome
      filtered = filterSelect [] env
  filtered @?= env

testSelectAliases :: IO ()
testSelectAliases = do
  -- The skill docs and the MCP schema tell agents to pass
  -- @--select sig,haddock@; the result fields are spelled @signature@
  -- and @haddock_raw@.  Both spellings must mean the same fields, or
  -- the documented invocation returns an empty @result: {}@.
  parseSelectList "sig,haddock" @?= ["signature", "haddock_raw"]
  parseSelectList "signature,haddock_raw" @?= ["signature", "haddock_raw"]
  parseSelectList "name, sig" @?= ["name", "signature"]
  parseSelectList "" @?= []

testUnmatchedSelect :: IO ()
testUnmatchedSelect = do
  -- Without this report the projection just drops a name it cannot
  -- satisfy, so a typo — or a field that only exists under --full, or
  -- one belonging to a different command — answers @result: {}@ with
  -- exit 0 and no explanation.
  let card     = SymbolCard "x" "fn" "p" "1.0" "M"
                   (Just "x :: Int") (Just " doc") Nothing
      compactK = Set.fromList ["name", "kind", "package", "version", "module"]
      fullK    = Set.union compactK (Set.fromList ["signature", "haddock_raw"])
      ask sel full =
        unmatchedSelect
          defaultEnvelopeOpts { eoSelect = parseSelectList sel, eoFull = full }
          compactK fullK
          (successOutcome SymbolCmd
             (if full then toFullJSON card else toCompactJSON card))

  -- A field the command does answer: nothing to report.
  fst (ask "name" False) @?= []

  -- A name no command produces, alongside a good one: only the bad one.
  ask "name,bogus" False @?= (["bogus"], compactK)

  -- A real field that this field set does not carry.  `sig` canonicalises
  -- to `signature`, which lives in the full set only, so asking for it
  -- without --full is a miss and the report says so in wire spelling.
  ask "sig" False @?= (["signature"], compactK)

  -- Same request under --full lands.
  fst (ask "sig" True) @?= []

testSelectAliasesProject :: IO ()
testSelectAliasesProject = do
  let result = toFullJSON (SymbolCard "x" "fn" "p" "1.0" "M"
                            (Just "x :: Int") (Just " doc") Nothing)
      kept   = objectKeys (filterSelect (parseSelectList "sig,haddock") result)
  kept @?= Set.fromList ["signature", "haddock_raw"]
