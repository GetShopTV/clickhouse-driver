{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

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
  , encodeValueWithSettings
  , encodeRowWithSettings
  , encodeRowsWithSettings
  ) where

import Control.Exception (throw)
import Data.Bits (shiftR, (.&.), (.|.))
import Data.Aeson qualified as Aeson
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
import Data.Time (Day)
import Data.Time.Calendar (diffDays)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Clock.System (systemEpochDay)
import Data.UUID (toWords64)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Word (Word64)
import Database.Clickhouse.Client.Types (ClickhouseType (..), ClickhouseSettingsException (..), settingEnabled)

-- | Encode one value.
encodeValue :: ClickhouseType -> Builder
encodeValue = encodeValueWithSettings []

encodeValueWithSettings :: [(ByteString, ByteString)] -> ClickhouseType -> Builder
encodeValueWithSettings params = \case
  ClickString bs -> encodeLEB128 (fromIntegral (BS.length bs)) <> byteString bs
  ClickFixedString bs -> byteString bs
  ClickBool True -> word8 1
  ClickBool False -> word8 0
  ClickInt8 n -> int8 n
  ClickInt16 n -> int16LE n
  ClickInt32 n -> int32LE n
  ClickInt64 n -> int64LE n
  ClickInt128 n -> encodeLEInteger 16 (toInteger n)
  ClickInt256 n -> encodeLEInteger 32 (toInteger n)
  ClickUInt8 n -> word8 n
  ClickUInt16 n -> word16LE n
  ClickUInt32 n -> word32LE n
  ClickUInt64 n -> word64LE n
  ClickUInt128 n -> encodeLEInteger 16 (toInteger n)
  ClickUInt256 n -> encodeLEInteger 32 (toInteger n)
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
  ClickDecimal128 n -> encodeLEInteger 16 n
  ClickDecimal256 n -> encodeLEInteger 32 n
  ClickIPv4 address -> word32LE address
  ClickIPv6 bytes
    | BS.length bytes /= 16 -> error "ClickIPv6: expected 16 bytes"
    | otherwise -> byteString bytes
  ClickJSON value
    | settingEnabled "input_format_binary_read_json_as_string" params ->
        let bytes = BSL.toStrict (Aeson.encode value)
         in encodeLEB128 (fromIntegral (BS.length bytes)) <> byteString bytes
    | otherwise -> throw (ClickhouseSettingsException "JSON RowBinary encoding requires input_format_binary_read_json_as_string=1; enable it explicitly in settings")
  ClickNullable Nothing -> word8 1
  ClickNullable (Just value) -> word8 0 <> encodeValueWithSettings params value
  ClickArray values ->
    encodeLEB128 (fromIntegral (Vector.length values))
      <> foldMap (encodeValueWithSettings params) values
  ClickTuple values -> foldMap (encodeValueWithSettings params) values
  ClickMap entries ->
    encodeLEB128 (fromIntegral (Vector.length entries))
      <> foldMap (\(k, v) -> encodeValueWithSettings params k <> encodeValueWithSettings params v) entries

-- | Encode a single row (the concatenation of its columns).
encodeRow :: Vector ClickhouseType -> Builder
encodeRow = encodeRowWithSettings []

encodeRowWithSettings :: [(ByteString, ByteString)] -> Vector ClickhouseType -> Builder
encodeRowWithSettings params = foldMap (encodeValueWithSettings params)

-- | Encode many rows into one strict payload, suitable as the body of an
-- @INSERT ... FORMAT RowBinary@ request.
encodeRows :: [Vector ClickhouseType] -> ByteString
encodeRows = encodeRowsWithSettings []

encodeRowsWithSettings :: [(ByteString, ByteString)] -> [Vector ClickhouseType] -> ByteString
encodeRowsWithSettings params rows =
  BSL.toStrict (toLazyByteString (foldMap (encodeRowWithSettings params) rows))

-- | Encode an integer as two's complement, little-endian, over the given
-- number of bytes. Negative values are wrapped to their fixed-width
-- representation.
encodeLEInteger :: Int -> Integer -> Builder
encodeLEInteger widthBytes value =
  mconcat
    [ word64LE (fromIntegral (wrapped `shiftR` (64 * chunk)) :: Word64)
    | chunk <- [0 .. widthBytes `div` 8 - 1]
    ]
  where
    bits = widthBytes * 8
    wrapped = value `mod` 2 ^ bits

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
