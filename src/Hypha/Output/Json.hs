{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
module Hypha.Output.Json
  ( ToOutcomeJson (..)
  , EnvelopeOpts (..)
  , defaultEnvelopeOpts
  , encodeEnvelope
  , encodeOutcomeBytes
  , filterSelect
  , restrictBody
  , objectKeys
  , parseSelectList
  ) where

import Data.Aeson
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LBS
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text

import Hypha.Output.Outcome (Outcome (..), OutcomeError (..), Related (..))
import Hypha.Cli.Types

-- | Two field-set variants: compact (default) and full (--full).
-- Each command result type implements this class.
class ToOutcomeJson a where
  toCompactJSON :: a -> Value
  toFullJSON    :: a -> Value

-- | Options that control how an outcome envelope is serialised.
data EnvelopeOpts = EnvelopeOpts
  { eoFull       :: !Bool
    -- ^ When true, use the full field set; otherwise restrict to compact.
  , eoSelect     :: ![Text]
    -- ^ When non-empty, post-filter top-level result keys.
  , eoPrettyJson :: !Bool
    -- ^ When true, emit indented JSON; otherwise compressed.
  }
  deriving stock (Show, Eq)

-- | Default options: compact field set, no select projection, compressed JSON.
defaultEnvelopeOpts :: EnvelopeOpts
defaultEnvelopeOpts = EnvelopeOpts
  { eoFull       = False
  , eoSelect     = []
  , eoPrettyJson = False
  }

-- | Build the envelope 'Value' from an already-serialised command result.
--
-- This is the low-level builder; most callers should use 'encodeOutcomeBytes',
-- which additionally projects fields and chooses compact vs pretty encoding.
encodeEnvelope :: ClientCommandTag -> Outcome Value -> Value
encodeEnvelope cmdName = \case
  OutcomeSuccess result outside overrides actions related ->
    object
      [ "schema"       .= ("hypha/v0" :: Text)
      , "command"      .= clientCommandName cmdName
      , "ok"           .= True
      , "outside_plan" .= outside
      , "overrides"    .= overrides
      , "result"       .= result
      , "actions"      .= actions
      , "related"      .= map toRelatedObject related
      ]
  OutcomeFailure err actions ->
    object
      [ "schema"   .= ("hypha/v0" :: Text)
      , "command"  .= clientCommandName cmdName
      , "ok"       .= False
      , "error"    .= object
          [ "code"      .= oeCode err
          , "message"   .= oeMessage err
          , "exit_code" .= oeExitCode err
          ]
      , "actions"  .= actions
      ]
  where
    toRelatedObject :: Related -> Value
    toRelatedObject r = object
      [ "label" .= relatedLabel r
      , "fetch" .= relatedFetch r
      ]

-- | Serialise an outcome to a 'LBS.ByteString', honouring the envelope options.
--
-- The two @Set Text@ arguments are the per-command compact and full key
-- sets.  The compact set must be a (non-strict) subset of the full set; this
-- invariant is enforced by 'Property.OutputJson'.  Pass equal sets if there
-- is no real distinction yet for a particular command.
encodeOutcomeBytes
  :: EnvelopeOpts
  -> ClientCommandTag -- ^ command name
  -> Set Text         -- ^ compact key set for the result body
  -> Set Text         -- ^ full key set for the result body
  -> Outcome Value
  -> LBS.ByteString
encodeOutcomeBytes opts cmdName compact full oc =
  let projected = projectOutcome opts compact full oc
      envelope  = encodeEnvelope cmdName projected
  in if eoPrettyJson opts
       then AesonPretty.encodePretty envelope
       else encode envelope

-- | Apply the field-set projection and --select projection to the success body.
projectOutcome :: EnvelopeOpts -> Set Text -> Set Text -> Outcome Value -> Outcome Value
projectOutcome opts compact full = \case
  OutcomeSuccess result outside overrides actions related ->
    let keep1 = if eoFull opts then full else compact
        step1 = restrictKeys keep1 result
        step2 = case eoSelect opts of
                  [] -> step1
                  ks -> restrictKeys (Set.fromList ks) step1
    in OutcomeSuccess step2 outside overrides actions related
  failure -> failure

-- | Internal: keep only the listed keys at the top level of an 'Object'.
-- Pass-through on non-objects so primitive results are not silently dropped.
restrictKeys :: Set Text -> Value -> Value
restrictKeys keys (Object km) =
  Object (KM.filterWithKey (\k _ -> Key.toText k `Set.member` keys) km)
restrictKeys _ v = v

-- | Post-filter a JSON value, keeping only the listed top-level keys.
-- When the key list is empty the value passes through unchanged.
filterSelect :: [Text] -> Value -> Value
filterSelect []   = id
filterSelect keys = restrictBody keys

-- | Restrict an object to the given keys.  Non-objects pass through.
restrictBody :: [Text] -> Value -> Value
restrictBody keys (Object obj) =
  let keep = Set.fromList (map (Key.fromText . Text.strip) keys)
  in Object (KM.filterWithKey (\k _ -> k `Set.member` keep) obj)
restrictBody _ v = v

-- | Extract the set of top-level keys from a JSON 'Value'.  Non-objects
-- yield the empty set.
objectKeys :: Value -> Set Text
objectKeys (Object obj) = Set.fromList (map Key.toText (KM.keys obj))
objectKeys _            = Set.empty

-- | Split a comma-separated select-list, trimming whitespace and dropping
-- empty entries.
parseSelectList :: Text -> [Text]
parseSelectList = filter (not . Text.null) . map Text.strip . Text.splitOn ","
