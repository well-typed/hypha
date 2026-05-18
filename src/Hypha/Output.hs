{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Output
  ( -- * Types
    OutcomeEnvelope (..)
    -- * Construction
  , successEnvelope
  , errorEnvelope
    -- * Encoding
  , encodeEnvelope
  ) where

import Data.Aeson (Value (..), (.=))
import qualified Data.Aeson as Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)

import Hypha.Error (HyphaError (..), errorToExitCode, errorToMessage)
import Hypha.Exit (ExitCode (..))

-- | The top-level JSON envelope emitted by every command.
data OutcomeEnvelope = OutcomeEnvelope
  { oeSchema     :: !Text
    -- ^ Always @"hypha/v0"@.
  , oeCommand    :: !Text
    -- ^ The command name.
  , oeOk         :: !Bool
    -- ^ Whether the command succeeded.
  , oeOutsidePlan :: !Bool
    -- ^ Whether the result is outside the build plan.
  , oeOverrides  :: ![Text]
    -- ^ Active package overrides.
  , oeResult     :: !(Maybe Value)
    -- ^ Command-specific result (present on success).
  , oeError      :: !(Maybe Value)
    -- ^ Error details (present on failure).
  , oeActions    :: !(Maybe Value)
    -- ^ Suggested next actions.
  , oeRelated    :: !(Maybe Value)
    -- ^ Related items.
  }
  deriving stock (Show)

-- | Create a success envelope.
successEnvelope :: Text -> [Text] -> Value -> OutcomeEnvelope
successEnvelope cmd overrides result = OutcomeEnvelope
  { oeSchema      = "hypha/v0"
  , oeCommand     = cmd
  , oeOk          = True
  , oeOutsidePlan = False
  , oeOverrides   = overrides
  , oeResult      = Just result
  , oeError       = Nothing
  , oeActions     = Nothing
  , oeRelated     = Nothing
  }

-- | Create an error envelope.
errorEnvelope :: Text -> [Text] -> HyphaError -> OutcomeEnvelope
errorEnvelope cmd overrides err = OutcomeEnvelope
  { oeSchema      = "hypha/v0"
  , oeCommand     = cmd
  , oeOk          = False
  , oeOutsidePlan = False
  , oeOverrides   = overrides
  , oeResult      = Nothing
  , oeError       = Just errorObj
  , oeActions     = Nothing
  , oeRelated     = Nothing
  }
  where
    errorObj = Aeson.object
      [ "code"      .= errorCodeToText err
      , "message"   .= errorToMessage err
      , "exit_code" .= unExitCode (errorToExitCode err)
      ]

-- | Map error constructors to their code strings.
errorCodeToText :: HyphaError -> Text
errorCodeToText (CliError _)          = "CLI_ERROR"
errorCodeToText (NotFound _)          = "NOT_FOUND"
errorCodeToText (NetworkError _)      = "NETWORK_ERROR"
errorCodeToText (CacheError _)        = "CACHE_ERROR"
errorCodeToText (EnvironmentError _)  = "ENVIRONMENT_ERROR"

-- | Encode an envelope to JSON bytes.
encodeEnvelope :: OutcomeEnvelope -> ByteString
encodeEnvelope = Aeson.encode

-- ToJSON instance for OutcomeEnvelope
instance Aeson.ToJSON OutcomeEnvelope where
  toJSON env = Aeson.object $ concat
    [ [ "schema"        .= oeSchema env
      , "command"       .= oeCommand env
      , "ok"            .= oeOk env
      , "outside_plan"  .= oeOutsidePlan env
      , "overrides"     .= oeOverrides env
      ]
    , maybe [] (\r -> ["result" .= r]) (oeResult env)
    , maybe [] (\e -> ["error" .= e]) (oeError env)
    , maybe [] (\a -> ["actions" .= a]) (oeActions env)
    , maybe [] (\r -> ["related" .= r]) (oeRelated env)
    ]
