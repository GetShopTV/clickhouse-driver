{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

{- |
Streaming RowBinary decoding.

The conduit entry point expects the @RowBinaryWithNamesAndTypes@ response
layout: a LEB128 encoded column count, then the column names and the column
types (each as a length-prefixed RowBinary string), followed by the binary
rows.  Rows are decoded and yielded one at a time as data arrives, so large
result sets can be consumed incrementally.

Rows are returned as 'Vector' of dynamically typed 'ClickhouseType' values
whose constructors are chosen from the type names in the response header.
-}
module Database.Clickhouse.Conversion.Binary.Decode
  ( decodeRowBinaryC
  , decodeRowsFromSchema
  , decodeRowBinaryBuffer
  ) where

import Control.Exception (throwIO)
import Control.Monad (replicateM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Bits (shiftL, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Conduit (ConduitT, await, yield)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Time (Day, UTCTime)
import Data.Time.Calendar (addDays)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Clock.System (systemEpochDay)
import Data.UUID (fromWords64)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Word (Word16, Word32, Word64, Word8)
import Database.Clickhouse.Client.Types
  ( ClickhouseDecodeException (..)
  , ClickhouseType (..)
  )
import Database.Clickhouse.Conversion.Types
  ( ChType (..)
  , decimalWidthBytes
  , parseChType
  )
import GHC.Float (castWord32ToFloat, castWord64ToDouble)

-- | Result of running a parser over the bytes available so far.
data PResult a
  = PDone !a !ByteString -- ^ value and the unconsumed remainder
  | PNeedMore -- ^ the value needs more input
  | PFail !String

-- | A small pure parser over strict 'ByteString' buffers.  It never consumes
-- input when returning 'PNeedMore', which lets the driver replay the parse of
-- an incomplete value once the next chunk has arrived.
newtype P a = P {runP :: ByteString -> PResult a}

instance Functor P where
  fmap f (P g) = P $ \bs -> case g bs of
    PDone a rest -> PDone (f a) rest
    PNeedMore -> PNeedMore
    PFail err -> PFail err

instance Applicative P where
  pure a = P $ \bs -> PDone a bs
  P f <*> P x = P $ \bs -> case f bs of
    PDone g rest -> case x rest of
      PDone a rest' -> PDone (g a) rest'
      PNeedMore -> PNeedMore
      PFail err -> PFail err
    PNeedMore -> PNeedMore
    PFail err -> PFail err

instance Monad P where
  P f >>= k = P $ \bs -> case f bs of
    PDone a rest -> runP (k a) rest
    PNeedMore -> PNeedMore
    PFail err -> PFail err

pNeed :: Int -> (ByteString -> a) -> P a
pNeed n readValue = P $ \bs ->
  if BS.length bs >= n
    then PDone (readValue bs) (BS.drop n bs)
    else PNeedMore

pWord8 :: P Word8
pWord8 = pNeed 1 (\bs -> BS.index bs 0)

pInt8 :: P Int8
pInt8 = fromIntegral <$> pWord8

pWord16le :: P Word16
pWord16le =
  pNeed 2 $ \bs ->
    fromIntegral (BS.index bs 0)
      .|. (fromIntegral (BS.index bs 1) `shiftL` 8)

pInt16le :: P Int16
pInt16le = fromIntegral <$> pWord16le

pWord32le :: P Word32
pWord32le =
  pNeed 4 $ \bs ->
    foldWord32
      [ BS.index bs 0
      , BS.index bs 1
      , BS.index bs 2
      , BS.index bs 3
      ]

pInt32le :: P Int32
pInt32le = fromIntegral <$> pWord32le

pWord64le :: P Word64
pWord64le =
  pNeed 8 $ \bs ->
    foldWord64
      [ BS.index bs 0
      , BS.index bs 1
      , BS.index bs 2
      , BS.index bs 3
      , BS.index bs 4
      , BS.index bs 5
      , BS.index bs 6
      , BS.index bs 7
      ]

pInt64le :: P Int64
pInt64le = fromIntegral <$> pWord64le

pFloat32le :: P Float
pFloat32le = castWord32ToFloat <$> pWord32le

pFloat64le :: P Double
pFloat64le = castWord64ToDouble <$> pWord64le

pBytes :: Int -> P ByteString
pBytes n = pNeed n (BS.take n)

-- | Length-prefixed string: unsigned LEB128 length, then the bytes.
pStringBytes :: P ByteString
pStringBytes = do
  len <- pLEB128
  pBytes (fromIntegral len)

pLEB128 :: P Word64
pLEB128 = P $ go 0 0
  where
    go !acc !shift bs = case BS.uncons bs of
      Nothing -> PNeedMore
      Just (byte, rest) ->
        let !value = acc .|. (fromIntegral (byte .&. 0x7F) `shiftL` shift)
         in if byte .&. 0x80 == 0
              then PDone value rest
              else go value (shift + 7) rest

foldWord32 :: [Word8] -> Word32
foldWord32 = go 0 0
  where
    go !acc _ [] = acc
    go !acc !shift (w : ws) =
      go (acc .|. (fromIntegral w `shiftL` shift)) (shift + 8) ws

foldWord64 :: [Word8] -> Word64
foldWord64 = go 0 0
  where
    go !acc _ [] = acc
    go !acc !shift (w : ws) =
      go (acc .|. (fromIntegral w `shiftL` shift)) (shift + 8) ws

-- | Decode one column according to its type.
decodeValue :: ChType -> P ClickhouseType
decodeValue = \case
  ChInt8 -> ClickInt8 <$> pInt8
  ChInt16 -> ClickInt16 <$> pInt16le
  ChInt32 -> ClickInt32 <$> pInt32le
  ChInt64 -> ClickInt64 <$> pInt64le
  ChUInt8 -> ClickUInt8 <$> pWord8
  ChUInt16 -> ClickUInt16 <$> pWord16le
  ChUInt32 -> ClickUInt32 <$> pWord32le
  ChUInt64 -> ClickUInt64 <$> pWord64le
  ChFloat32 -> ClickFloat32 <$> pFloat32le
  ChFloat64 -> ClickFloat64 <$> pFloat64le
  ChBool -> ClickBool . (/= 0) <$> pWord8
  ChString -> ClickString <$> pStringBytes
  ChFixedString n -> ClickFixedString <$> pBytes n
  ChDate -> ClickDate . dayFromDays . fromIntegral <$> pWord16le
  ChDate32 -> ClickDate32 . dayFromDays . fromIntegral <$> pInt32le
  ChDateTime -> ClickDateTime . secondsToUTC . fromIntegral <$> pInt32le
  ChDateTime64 precision -> do
    scaled <- pInt64le
    pure (ClickDateTime64 precision (scaledSecondsToUTC precision scaled))
  ChDecimal precision _ -> do
    let width = decimalWidthBytes precision
    mantissa <- case width of
      4 -> fromIntegral <$> pInt32le
      8 -> fromIntegral <$> pInt64le
      _ -> signed128 <$> ((,) <$> pWord64le <*> pWord64le)
    pure $ case width of
      4 -> ClickDecimal32 mantissa
      8 -> ClickDecimal64 mantissa
      _ -> ClickDecimal128 mantissa
  ChUuid -> do
    hi <- pWord64le
    lo <- pWord64le
    pure (ClickUuid (fromWords64 hi lo))
  ChEnum 8 -> ClickInt8 <$> pInt8
  ChEnum 16 -> ClickInt16 <$> pInt16le
  ChEnum _ -> ClickInt64 <$> pInt64le
  ChNullable inner -> do
    isNull <- pWord8
    if isNull /= 0
      then pure (ClickNullable Nothing)
      else ClickNullable . Just <$> decodeValue inner
  ChLowCardinality inner -> decodeValue inner
  ChArray inner -> do
    len <- pLEB128
    ClickArray . Vector.fromList <$> replicateM (fromIntegral len) (decodeValue inner)
  ChTuple inners -> ClickTuple . Vector.fromList <$> mapM decodeValue inners
  ChMap keyType valueType -> do
    len <- pLEB128
    ClickMap . Vector.fromList
      <$> replicateM
        (fromIntegral len)
        ((,) <$> decodeValue keyType <*> decodeValue valueType)

decodeRow :: [ChType] -> P (Vector ClickhouseType)
decodeRow schemas = Vector.fromList <$> mapM decodeValue schemas

dayFromDays :: Integer -> Day
dayFromDays days = addDays days systemEpochDay

secondsToUTC :: Integer -> UTCTime
secondsToUTC = posixSecondsToUTCTime . fromIntegral

scaledSecondsToUTC :: Int -> Int64 -> UTCTime
scaledSecondsToUTC precision scaled =
  posixSecondsToUTCTime
    (fromRational (fromIntegral scaled / (10 ^ precision :: Rational)))

signed128 :: (Word64, Word64) -> Integer
signed128 (lo, hi) =
  let raw = toInteger lo + (toInteger hi `shiftL` 64)
      modulus = 2 ^ (128 :: Int)
   in if raw >= modulus `div` 2
        then raw - modulus
        else raw

headerParser :: P ([ByteString], [ByteString])
headerParser = do
  columnCount <- pLEB128
  names <- replicateM (fromIntegral columnCount) pStringBytes
  types <- replicateM (fromIntegral columnCount) pStringBytes
  pure (names, types)

parseTypes :: [ByteString] -> Either String [ChType]
parseTypes = mapM parseChType

-- | Stream rows from @RowBinaryWithNamesAndTypes@ response chunks.
--
-- An empty body (a result with no rows, or a statement with no output) is a
-- valid stream that yields nothing.
decodeRowBinaryC :: (MonadIO m) => ConduitT ByteString (Vector ClickhouseType) m ()
decodeRowBinaryC = header mempty
  where
    header !acc = do
      next <- await
      let !buffer = maybe acc (acc <>) next
      case runP headerParser buffer of
        PFail err -> throwDecode err
        PNeedMore -> case next of
          Nothing
            | BS.null buffer -> pure ()
            | otherwise -> throwDecode "truncated header"
          Just _ -> header buffer
        PDone (_, types) rest -> do
          schemas <- either throwDecode pure (parseTypes types)
          rows schemas rest

    rows schemas !acc = do
      case runP (decodeRow schemas) acc of
        PFail err -> throwDecode err
        PNeedMore -> do
          next <- await
          case next of
            Nothing -> throwDecode "unexpected end of input in the middle of a row"
            Just chunk -> rows schemas (acc <> chunk)
        PDone row rest -> do
          yield row
          if BS.null rest
            then do
              next <- await
              case next of
                Nothing -> pure ()
                Just chunk -> rows schemas chunk
            else rows schemas rest

throwDecode :: (MonadIO m) => String -> m a
throwDecode = liftIO . throwIO . ClickhouseDecodeException

-- | Decode the rows of a complete @RowBinaryWithNamesAndTypes@ buffer.
decodeRowBinaryBuffer :: ByteString -> Either String [Vector ClickhouseType]
decodeRowBinaryBuffer buffer = case runP headerParser buffer of
  PFail err -> Left err
  PNeedMore -> Left "truncated header"
  PDone (_, types) rest -> do
    schemas <- parseTypes types
    strictRows schemas rest

-- | Decode rows from a complete buffer given the column types.
decodeRowsFromSchema :: [ChType] -> ByteString -> Either String [Vector ClickhouseType]
decodeRowsFromSchema schemas = strictRows schemas

strictRows :: [ChType] -> ByteString -> Either String [Vector ClickhouseType]
strictRows schemas = go
  where
    go !buffer =
      if BS.null buffer
        then Right []
        else case runP (decodeRow schemas) buffer of
          PDone row rest -> (row :) <$> go rest
          PNeedMore -> Left "unexpected end of input in the middle of a row"
          PFail err -> Left err
