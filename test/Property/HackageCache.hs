{-# LANGUAGE OverloadedStrings #-}
module Property.HackageCache (tests) where

import Data.Aeson (encode, decode)
import qualified Data.ByteString as BS
import Data.Word (Word8)
import Data.Time (UTCTime(..), secondsToNominalDiffTime)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (secondsToDiffTime)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual)
import Test.Tasty.Falsify (testProperty)
import qualified Test.Falsify.Generator as Gen
import qualified Test.Falsify.Range as Range
import Test.Falsify.Property (gen, assert)
import qualified Test.Falsify.Predicate as P

import Hypha.Hackage.Types (CacheKind(..), CachedResponse(..))

-- | Generate a CacheKind for testing.
genCacheKind :: Gen.Gen CacheKind
genCacheKind = do
  isImmutable <- Gen.bool False
  if isImmutable
    then pure Immutable
    else do
      ttl :: Int <- Gen.inRange (Range.between (1, 86400))
      pure (TtlMutable (secondsToNominalDiffTime (fromIntegral ttl)))

-- | Generate a ByteString for testing.
genByteString :: Gen.Gen BS.ByteString
genByteString = do
  len :: Word <- Gen.inRange (Range.between (0, 100))
  bytes <- Gen.list (Range.between (0, len))
                    (Gen.inRange (Range.between (0 :: Int, 255)))
  pure (BS.pack (map (fromIntegral :: Int -> Word8) bytes))

-- | Generate a Maybe ByteString.
genMaybeByteString :: Gen.Gen (Maybe BS.ByteString)
genMaybeByteString = do
  has <- Gen.bool False
  if has
    then Just <$> genByteString
    else pure Nothing

-- | Generate a UTCTime for testing.
genUTCTime :: Gen.Gen UTCTime
genUTCTime = do
  sec :: Int <- Gen.inRange (Range.between (0, 86399))
  pure UTCTime
    { utctDay = fromGregorian 2024 1 1
    , utctDayTime = secondsToDiffTime (fromIntegral sec)
    }

-- | Generate a CachedResponse for testing.
genCachedResponse :: Gen.Gen CachedResponse
genCachedResponse = do
  etag <- genMaybeByteString
  hasLastMod <- Gen.bool False
  lastMod <- if hasLastMod then Just <$> genUTCTime else pure Nothing
  storedAt <- genUTCTime
  body <- genByteString
  kind <- genCacheKind
  pure CachedResponse
    { crEtag = etag
    , crLastModified = lastMod
    , crStoredAt = storedAt
    , crBody = body
    , crKind = kind
    }

tests :: TestTree
tests = testGroup "Property.HackageCache"
  [ testProperty "CachedResponse JSON roundtrip" $ do
      resp <- gen genCachedResponse
      let encoded = encode resp
      case decode encoded of
        Nothing -> fail "Failed to decode CachedResponse"
        Just resp' ->
          assert $ P.eq P..$ ("expected", resp) P..$ ("got", resp')

  , testCase "CacheKind Immutable JSON roundtrip" $ do
      let kind = Immutable
      let encoded = encode kind
      let decoded = decode encoded
      assertEqual "CacheKind roundtrip" (Just kind) decoded

  , testCase "CacheKind TtlMutable JSON roundtrip" $ do
      let kind = TtlMutable (secondsToNominalDiffTime 900)
      let encoded = encode kind
      let decoded = decode encoded
      assertEqual "CacheKind roundtrip" (Just kind) decoded
  ]
