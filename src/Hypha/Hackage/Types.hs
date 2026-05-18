{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
module Hypha.Hackage.Types
  ( CacheKind (..)
  , CachedResponse (..)
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import qualified Data.ByteString as BS
import Data.Char (intToDigit, digitToInt)
import Data.Time (UTCTime, NominalDiffTime)
import Data.Word (Word8)
import GHC.Generics (Generic)

-- | Whether a cached resource is immutable (specific version) or mutable
-- (latest metadata) with a TTL.
data CacheKind
  = Immutable
  | TtlMutable !NominalDiffTime
  deriving stock (Show, Eq, Generic)

instance FromJSON CacheKind
instance ToJSON CacheKind

-- | Encode a ByteString as a hex string for JSON serialization.
encodeBytesHex :: BS.ByteString -> String
encodeBytesHex = concatMap (\b -> let w = fromIntegral b in [intToDigit (w `div` 16), intToDigit (w `mod` 16)]) . BS.unpack

-- | Decode a hex string back to a ByteString.
-- Returns Left with an error message on failure.
decodeBytesHex :: String -> Either String BS.ByteString
decodeBytesHex s
  | odd (length s) = Left "hex string has odd length"
  | otherwise = go s
  where
    go [] = Right BS.empty
    go (a:b:rest) =
      case (hexDigit a, hexDigit b) of
        (Just ha, Just hb) -> do
          let byte = fromIntegral (ha * 16 + hb) :: Word8
          rest' <- go rest
          Right (BS.cons byte rest')
        _ -> Left ("invalid hex character in: " ++ [a, b])
    go _ = Left "impossible: odd length not caught"

    hexDigit c
      | c >= '0' && c <= '9' = Just (digitToInt c)
      | c >= 'a' && c <= 'f' = Just (digitToInt c - digitToInt 'a' + 10)
      | c >= 'A' && c <= 'F' = Just (digitToInt c - digitToInt 'A' + 10)
      | otherwise = Nothing

-- | A cached HTTP response with revalidation metadata.
data CachedResponse = CachedResponse
  { crEtag         :: !(Maybe BS.ByteString)
  , crLastModified :: !(Maybe UTCTime)
  , crStoredAt     :: !UTCTime
  , crBody         :: !BS.ByteString
  , crKind         :: !CacheKind
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON CachedResponse where
  toJSON cr = object
    [ "etag"         .= fmap encodeBytesHex (crEtag cr)
    , "lastModified" .= crLastModified cr
    , "storedAt"     .= crStoredAt cr
    , "body"         .= encodeBytesHex (crBody cr)
    , "kind"         .= crKind cr
    ]

instance FromJSON CachedResponse where
  parseJSON = withObject "CachedResponse" $ \o -> do
    etagHex <- o .: "etag"
    lastMod <- o .: "lastModified"
    stored <- o .: "storedAt"
    bodyHex <- o .: "body"
    kind <- o .: "kind"
    body <- either fail pure (decodeBytesHex bodyHex)
    etagDecoded <- traverse (either fail pure . decodeBytesHex) etagHex
    pure CachedResponse
      { crEtag = etagDecoded
      , crLastModified = lastMod
      , crStoredAt = stored
      , crBody = body
      , crKind = kind
      }
