{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Hypha.Mcp.Server
  ( runMcpStdio
  , execHypha
  ) where

import Control.Exception (try, SomeException)
import Control.Monad (unless)
import Data.Aeson
  ( FromJSON (..), ToJSON (..), Value (..)
  , object, (.=), (.:), (.:?)
  , eitherDecodeStrict, encode, withObject
  )
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import Data.Vector (toList)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.IO (hFlush, hIsEOF, stdin, stdout)
import System.Process.Typed (proc, readProcess)

-- ---------------------------------------------------------------------------
-- * JSON-RPC types

-- | A JSON-RPC request envelope (method call or notification).
--
-- Notifications lack an @id@; we track that in 'rpcIsNotification'.
data JSONRPCRequest = JSONRPCRequest
  { rpcJsonrpc        :: !Text
  , rpcId             :: !(Maybe Value)
  , rpcMethod         :: !Text
  , rpcParams         :: !(Maybe Value)
  , rpcIsNotification :: !Bool
  }
  deriving stock (Show, Eq)

instance FromJSON JSONRPCRequest where
  parseJSON = withObject "JSONRPCRequest" $ \o -> do
    v  <- o .:   "jsonrpc"
    mi <- o .:?  "id"
    m  <- o .:   "method"
    p  <- o .:?  "params"
    pure (JSONRPCRequest v mi m p (Nothing == mi))

instance ToJSON JSONRPCRequest where
  toJSON (JSONRPCRequest v mi m p _) = object
    $ maybe [] (\i -> [ "id" .= i ]) mi
    ++ [ "jsonrpc" .= v, "method" .= m ]
    ++ maybe [] (\x -> [ "params" .= x ]) p

-- | A JSON-RPC response envelope.
--
-- Notifications produce /no/ response, but we reuse the same datatype
-- internally for the success-path helper functions.
data JSONRPCResponse = JSONRPCResponse
  { respJsonrpc :: !Text
  , respId      :: !(Maybe Value)
  , respResult  :: !(Maybe Value)
  , respError   :: !(Maybe JSONRPCError)
  }
  deriving stock (Show, Eq)

instance ToJSON JSONRPCResponse where
  toJSON (JSONRPCResponse v mi r e) = object
    $ maybe [] (\i -> [ "id" .= i ]) mi
    ++ [ "jsonrpc" .= v ]
    ++ maybe [] (\x -> [ "result" .= x ]) r
    ++ maybe [] (\x -> [ "error"  .= x ]) e

jsonSuccess :: Maybe Value -> Value -> JSONRPCResponse
jsonSuccess mReqId result = JSONRPCResponse "2.0" mReqId (Just result) Nothing

jsonError :: Maybe Value -> Int -> Text -> JSONRPCResponse
jsonError mReqId code msg = JSONRPCResponse "2.0" mReqId Nothing (Just err)
  where
    err = JSONRPCError code msg Nothing

-- | Convenience: render a JSON-RPC error as a strict byte string.
encodeError :: Maybe Value -> Int -> Text -> ByteString
encodeError mId code msg = LBS.toStrict (encode (jsonError mId code msg))

-- | JSON-RPC error object.
data JSONRPCError = JSONRPCError
  { errCode    :: !Int
  , errMessage :: !Text
  , errData    :: !(Maybe Value)
  }
  deriving stock (Show, Eq)

instance ToJSON JSONRPCError where
  toJSON (JSONRPCError c m d) = object
    $ [ "code" .= c, "message" .= m ]
    ++ maybe [] (\x -> [ "data" .= x ]) d

-- ---------------------------------------------------------------------------
-- * Server

-- | Run the MCP stdio server until EOF on stdin.
--
-- Each line is parsed as a JSON-RPC message.  Notifications are
-- consumed silently (no response written).
runMcpStdio :: IO ()
runMcpStdio = loop
  where
    loop = do
      eof <- hIsEOF stdin
      if eof
        then pure ()
        else do
          line <- BS8.getLine
          case eitherDecodeStrict line of
            Left parseErr -> do
              -- Malformed JSON-RPC: emit a generic parse-error response.
              -- No request id available, so we omit it per spec.
              BS8.putStrLn (encodeError Nothing (-32700)
                              (Text.pack parseErr))
              hFlush stdout
              loop
            Right req -> do
              resp <- dispatch req
              unless (rpcIsNotification req) $ do
                BS8.putStrLn (encodeStrict resp)
                hFlush stdout
              loop

    encodeStrict :: JSONRPCResponse -> ByteString
    encodeStrict = LBS.toStrict . encode

-- | Dispatch a single JSON-RPC request.  Notifications produce no response.
dispatch :: JSONRPCRequest -> IO JSONRPCResponse
dispatch req
  | rpcIsNotification req = case rpcMethod req of
      "notifications/initialized" -> pure (jsonSuccess Nothing (object []))
      "notifications/cancelled"   -> pure (jsonSuccess Nothing (object []))
      _                         -> pure (jsonSuccess Nothing (object []))
  | otherwise = case rpcMethod req of
      "initialize"      -> pure (jsonSuccess (rpcId req) initializeResult)
      "tools/list"      -> pure (jsonSuccess (rpcId req) toolsListResult)
      "tools/call"      -> handleToolCall (rpcId req) (rpcParams req)
      other             -> pure (jsonError (rpcId req) (-32601)
                                  ("Method not found: " <> other))

-- | Initialize response payload.
initializeResult :: Value
initializeResult = object
  [ "protocolVersion" .= ("2024-11-05" :: Text)
  , "capabilities"  .= object
      [ "tools" .= object [ "listChanged" .= False ]
      ]
  , "serverInfo"    .= object
      [ "name"    .= ("hypha-mcp" :: Text)
      , "version" .= ("0.0.0" :: Text)
      ]
  ]

-- | Tools/list response payload.
toolsListResult :: Value
toolsListResult = object
  [ "tools" .= [ hyphaExecTool ]
  ]

hyphaExecTool :: Value
hyphaExecTool = object
  [ "name"        .= ("hypha.exec" :: Text)
  , "description" .= ("Run a hypha CLI command and return its JSON envelope." :: Text)
  , "inputSchema" .= object
      [ "type"       .= ("object" :: Text)
      , "properties" .= object
          [ "args" .= object
              [ "type"        .= ("array" :: Text)
              , "items"       .= object [ "type" .= ("string" :: Text) ]
              , "description" .= ("hypha CLI arguments as strings" :: Text)
              ]
          ]
      , "required" .= ([ "args" ] :: [Text])
      ]
  ]

-- ---------------------------------------------------------------------------
-- * Tool execution

-- | Handle a @tools/call@ request.
handleToolCall :: Maybe Value -> Maybe Value -> IO JSONRPCResponse
handleToolCall mReqId mParams = do
  let args = parseArgs mParams
  bin <- hyphaBinPath
  (ec, out, err) <- execHypha bin (map Text.unpack args)
  let exitCode = case ec of ExitSuccess -> 0; ExitFailure n -> n
  let result = object
        [ "content" .= [ object
            [ "type" .= ("text" :: Text)
            , "text" .= out
            ]]
        , "_meta" .= object
            [ "exitCode" .= exitCode
            , "stderr"   .= err
            ]
        , "isError" .= (exitCode /= 0)
        ]
  pure (jsonSuccess mReqId result)

-- | Extract the string array from @{"arguments": {"args": [...]}}@.
-- Also tolerates a flat @{"args": [...]}@ for testing convenience.
parseArgs :: Maybe Value -> [Text]
parseArgs = \case
  Nothing -> []
  Just (Object o) ->
    -- The MCP tools/call params have the tool arguments under "arguments".
    let inner = case KM.lookup "arguments" o of
          Just (Object ao) -> ao
          _                -> o
    in case KM.lookup "args" inner of
         Just (Array arr)  -> [ txt | String txt <- toList arr ]
         Just (String raw) -> [raw]
         _                 -> []
  Just _ -> []

-- | Discover the path to the @hypha@ binary.
--
-- Honors @HYPHA_BIN@ environment override; otherwise assumes the binary
-- is on @PATH@ as simply @hypha@.
hyphaBinPath :: IO FilePath
hyphaBinPath = maybe "hypha" id <$> lookupEnv "HYPHA_BIN"

-- | Spawn the @hypha@ CLI with the given arguments and capture stdout,
-- stderr, and the exit code.
execHypha :: FilePath -> [String] -> IO (ExitCode, Text, Text)
execHypha bin args = do
  result <- try @SomeException $ do
    (ec, out, err) <- readProcess (proc bin args)
    let decodeBS = TE.decodeUtf8 . LBS.toStrict
    pure (ec, decodeBS out, decodeBS err)
  case result of
    Left e  -> pure (ExitFailure 1, "", Text.pack (show e))
    Right r -> pure r
