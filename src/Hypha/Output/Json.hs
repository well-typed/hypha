{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
module Hypha.Output.Json
  ( ToOutcomeJson (..)
  , EnvelopeOpts (..)
  , defaultEnvelopeOpts
  , encodeSuccessEnvelope
  , encodeErrorEnvelope
  , encodeInternalErrorEnvelope
  , encodeOutcomeEnvelope
  , encodeOutcomeBytes
  , encodeEnvelopeValue
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
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text

import Hypha.Error
  ( HyphaError, errorActions, errorCode, errorExitCode, errorMessage )
import Hypha.Exit (exitInternalError, unExitCode)
import Hypha.Output.Outcome (Outcome (..))

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

-- | Build a success envelope 'Value' from an 'Outcome'.  The
-- envelope is purely structural: @result@ plus optional
-- @outside_plan@, @overrides@, and @actions@ fields.  No metadata
-- header — the consumer already knows what they invoked.
encodeSuccessEnvelope :: Outcome Value -> Value
encodeSuccessEnvelope oc =
  object $
    [ "result" .= outcomeResult oc ]
    <> [ "outside_plan" .= True | outcomeOutsidePlan oc ]
    <> [ "overrides" .= outcomeOverrides oc | not (null (outcomeOverrides oc)) ]
    <> [ "actions"   .= outcomeActions oc   | not (Map.null (outcomeActions oc)) ]

-- | Build an error envelope 'Value' from a 'HyphaError'
-- (code/message/exit_code via 'errorCode' / 'errorMessage'
-- / 'errorExitCode', recovery hints via 'errorActions').
encodeErrorEnvelope :: HyphaError -> Value
encodeErrorEnvelope err =
  object $
    [ "error" .= object
           [ "code"      .= errorCode err
           , "message"   .= errorMessage err
           , "exit_code" .= unExitCode (errorExitCode err)
           ]
    ]
    <> [ "actions" .= errorActions err | not (Map.null (errorActions err)) ]

-- | Build the envelope 'Value' for a crash — an exception that escaped
-- the library rather than a reified 'HyphaError'.  The exit code is
-- pinned to 'exitInternalError' — by definition there is no typed
-- error to derive one from.
encodeInternalErrorEnvelope
  :: Text  -- ^ rendered exception
  -> Value
encodeInternalErrorEnvelope msg =
  object $
    [ "error" .= object
           [ "code"      .= ("INTERNAL_ERROR" :: Text)
           , "message"   .= msg
           , "exit_code" .= unExitCode exitInternalError
           ]
    ]

-- | Build the envelope 'Value' /post-projection/.  The result is
-- structurally identical to what 'encodeOutcomeBytes' would write to
-- bytes — produce it once and feed it both to the JSON encoder and to
-- the human renderer, so neither path has to round-trip through a
-- 'LBS.ByteString' that could "fail" to decode.
encodeOutcomeEnvelope
  :: EnvelopeOpts
  -> Set Text         -- ^ compact key set for the result body
  -> Set Text         -- ^ full key set for the result body
  -> Outcome Value
  -> Value
encodeOutcomeEnvelope opts compact full =
  encodeSuccessEnvelope . projectOutcome opts compact full

-- | Serialise a pre-built envelope 'Value', honouring the
-- 'eoPrettyJson' flag.
encodeEnvelopeValue :: EnvelopeOpts -> Value -> LBS.ByteString
encodeEnvelopeValue opts envelope
  | eoPrettyJson opts = AesonPretty.encodePretty envelope
  | otherwise         = encode envelope

-- | Convenience: build the envelope and serialise it in one step.  Use
-- when you only need the bytes; use 'encodeOutcomeEnvelope' +
-- 'encodeEnvelopeValue' separately when the same envelope also drives
-- the human renderer.
encodeOutcomeBytes
  :: EnvelopeOpts
  -> Set Text
  -> Set Text
  -> Outcome Value
  -> LBS.ByteString
encodeOutcomeBytes opts compact full =
  encodeEnvelopeValue opts . encodeOutcomeEnvelope opts compact full

-- | Apply the field-set + --select projection to a success outcome.
projectOutcome
  :: EnvelopeOpts -> Set Text -> Set Text -> Outcome Value -> Outcome Value
projectOutcome opts compact full oc =
  let keep1 = if eoFull opts then full else compact
      step1 = restrictKeys keep1 (outcomeResult oc)
      step2 = case eoSelect opts of
                [] -> step1
                ks -> restrictKeys (Set.fromList ks) step1
  in oc { outcomeResult = step2 }

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
