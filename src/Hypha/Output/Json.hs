{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Output.Json
  ( ToOutcomeJson (..)
  , encodeEnvelope
  , filterSelect
  , restrictBody
  , objectKeys
  ) where

import Data.Aeson
import Data.Aeson.Key (Key)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text

import Hypha.Output.Outcome (Outcome (..), OutcomeError (..), Related (..))

-- | Two field-set variants: compact (default) and full (--full).
-- Each command result type implements this class.
class ToOutcomeJson a where
  toCompactJSON :: a -> Value
  toFullJSON    :: a -> Value

-- | Build the envelope 'Value' from an already-serialised command result.
encodeEnvelope :: Text -> Outcome Value -> Value
encodeEnvelope cmdName = \case
  OutcomeSuccess result outside overrides actions related ->
    object
      [ "schema"       .= Text.pack "hypha/v0"
      , "command"      .= cmdName
      , "ok"           .= True
      , "outside_plan" .= outside
      , "overrides"    .= overrides
      , "result"       .= result
      , "actions"      .= actions
      , "related"      .= map toRelatedObject related
      ]
  OutcomeFailure err actions ->
    object
      [ "schema"   .= Text.pack "hypha/v0"
      , "command"  .= cmdName
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

-- | Post-filter a JSON value, keeping only the listed top-level keys.
-- When the key list is empty the value passes through unchanged.
filterSelect :: [Text] -> Value -> Value
filterSelect []     = id
filterSelect keys   = restrictBody keys

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
