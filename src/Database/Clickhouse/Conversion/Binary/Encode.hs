{-# LANGUAGE LambdaCase #-}

{- |
RowBinary serialisation of 'ClickhouseType' values.

Encoding follows the ClickHouse @RowBinary@ wire format:

  * integers / floats are little-endian fixed width;
  * @String@ is a LEB128 length followed by the bytes;
  * @Nullable@ prefixes every value with a one-byte null flag (@1@ = NULL,
    @0@ = value);
  * @Array@ and @Map@ are prefixed with a LEB128 element count;
  * @Tuple@ is the concatenation of its elements.
-}
module Database.Clickhouse.Conversion.Binary.Encode
  ( encodeValue
  , encodeRow
  , encodeRows
  , encodeLEB128
  ) where

import Data.Bits (shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder
  ( Builder
  , byteString
  , doubleLE
  , floatLE
  , int16LE
  , int32LE
  , int64LE
  , int8
  , toLazyByteString
  , word16LE
  , word32LE
  , word64LE
  , word8
  )
import Data.ByteString.Lazy qualified as BSL
import Data.Time (Day, UTCTime)
import Data.Time.Calendar (diffDays)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Clock.System (systemEpochDay)
import Data.UUID (toWords64)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Word (Word64)
import Database.Clickhouse.Client.Types (ClickhouseType (..))

-- | Encode one value.
encodeValue :: ClickhouseType -> Builder
encodeValue = \case
  ClickString bs -> encodeLEB128 (fromIntegral (BS.length bs)) <> byteString bs
  ClickFixedString bs -> byteString bs
  ClickBool True -> word8 1
  ClickBool False -> word8 0
  ClickInt8 n -> int8 n
  ClickInt16 n -> int16LE n
  ClickInt32 n -> int32LE n
  ClickInt64 n -> int64LE n
  ClickUInt8 n -> word8 n
  ClickUInt16 n -> word16LE n
  ClickUInt32 n -> word32LE n
  ClickUInt64 n -> word64LE n
  ClickFloat32 f -> floatLE f
  ClickFloat64 d -> doubleLE d
  ClickDate day -> int16LE (fromIntegral (epochDays day))
  ClickDate32 day -> int32LE (fromIntegral (epochDays day))
  ClickDateTime time -> int32LE (round (utcTimeToPOSIXSeconds time))
  ClickDateTime64 precision time ->
    int64LE (round (utcTimeToPOSIXSeconds time * fromIntegral (10 ^ precision :: Int)))
  ClickUuid uuid ->
    let (hi, lo) = toWords64 uuid
     in word64LE hi <> word64LE lo
  ClickDecimal32 n -> int32LE (fromIntegral n)
  ClickDecimal64 n -> int64LE (fromIntegral n)
  ClickDecimal128 n -> encodeSigned128 n
  ClickNullable Nothing -> word8 1
  ClickNullable (Just value) -> word8 0 <> encodeValue value
  ClickArray values ->
    encodeLEB128 (fromIntegral (Vector.length values))
      <> foldMap encodeValue values
  ClickTuple values -> foldMap encodeValue values
  ClickMap entries ->
    encodeLEB128 (fromIntegral (Vector.length entries))
      <> foldMap (\(k, v) -> encodeValue k <> encodeValue v) entries

-- | Encode a single row (the concatenation of its columns).
encodeRow :: Vector ClickhouseType -> Builder
encodeRow = foldMap encodeValue

-- | Encode many rows into one strict payload, suitable as the body of an
-- @INSERT ... FORMAT RowBinary@ request.
encodeRows :: [Vector ClickhouseType] -> ByteString
encodeRows rows =
  BSL.toStrict (toLazyByteString (foldMap encodeRow rows))

encodeSigned128 :: Integer -> Builder
encodeSigned128 n =
  let modulo = 2 ^ (128 :: Int)
      wrapped = n `mod` modulo
      lo = fromIntegral (wrapped .&. 0xFFFFFFFFFFFFFFFF) :: Word64
      hi = fromIntegral (wrapped `shiftR` 64) :: Word64
   in word64LE lo <> word64LE hi

epochDays :: Day -> Integer
epochDays day = diffDays day systemEpochDay

-- | Unsigned LEB128 varint, as used by RowBinary for string / array lengths.
encodeLEB128 :: Word64 -> Builder
encodeLEB128 = go
  where
    go !value
      | value <= 127 = word8 (fromIntegral value)
      | otherwise =
          word8 (fromIntegral (value .&. 0x7F) .|. 0x80)
            <> go (value `shiftR` 7)
