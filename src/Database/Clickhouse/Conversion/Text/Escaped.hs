{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Rendering of 'ClickhouseType' values into the escaped text form ClickHouse
accepts for @{name:Type}@ query parameters.

ClickHouse resolves a query parameter with @deserializeTextEscaped@
(@ReplaceQueryParameterVisitor::resolveParameterValueAsField@), so parameter
values are text, never @RowBinary@: raw bytes or a file part named
@param_x@ are rejected by the server.  The literal forms rendered here were
verified against ClickHouse 26.8.5.13:

  * a 'ClickString' / 'ClickFixedString' is bare escaped bytes at the top
    level, but must be single-quoted (with @\\'@ for embedded quotes) inside
    'ClickArray' / 'ClickTuple' / 'ClickMap';
  * a NULL is @\\N@ at the top level but @NULL@ inside a container;
  * 'ClickDate' / 'ClickDate32' are bare @YYYY-MM-DD@ at the top level and
    quoted inside a container; 'ClickUuid', 'ClickIPv4' and 'ClickIPv6' are
    bare as well and quoted inside a container;
  * 'ClickDateTime' renders as epoch seconds and 'ClickDateTime64' as epoch
    seconds with a fractional part.  Both are time-zone independent, which the
    textual @YYYY-MM-DD hh:mm:ss@ form is not: the SQL placeholder carries a
    time zone that 'ClickhouseType' does not.  (Inside a container a bare
    integer would also be misread as seconds for @DateTime64@, so the
    fractional form is used in both contexts.)
  * 'ClickDecimal32' and friends cannot be rendered: the constructor carries
    the unscaled integer only, while the scale lives in the SQL placeholder
    type.  'ClickJSON' has no verified escaped-text form either.

The escaping rules for strings were verified byte by byte: a backslash (a raw
trailing one is a parse error), tab and newline (both stop the top-level parse)
are escaped as @\\\\@, @\\t@ and @\\n@, carriage return and NUL as @\\r@ and
@\\0@, every other byte below @0x20@ and @0x7F@ as @\\xHH@, and bytes
@>= 0x80@ are passed through verbatim.
-}
module Database.Clickhouse.Conversion.Text.Escaped
  ( renderQueryParamValue
  , effectiveRequestQueryParams
  ) where

import Data.Bits (shiftR, (.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.List (intercalate)
import Data.Time (Day, UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.Vector qualified as Vector
import Data.Word (Word32)
import Database.Clickhouse.Client.Types
  ( CHRequest (..)
  , ClickhouseSettingsException (..)
  , ClickhouseType (..)
  )
import Numeric (showHex)

-- | Render one value into the escaped text ClickHouse expects in a
-- @param_<name>@ field (or URL parameter).
renderQueryParamValue :: ClickhouseType -> Either ClickhouseSettingsException ByteString
renderQueryParamValue = renderValue False

-- | The @param_<name>@ bindings of a request, rendered in declaration order.
--
-- Fails when a binding cannot be rendered (see 'renderQueryParamValue'), when
-- a name is empty, when a name would corrupt the multipart field header, or
-- when the resulting @param_<name>@ is already taken by a raw entry in
-- 'requestParams' or in the connection 'settings'.
--
-- This lives next to the renderer rather than in
-- "Database.Clickhouse.Client.Types" because it needs
-- 'renderQueryParamValue', and the renderer already imports the type module.
effectiveRequestQueryParams ::
  [(String, ClickhouseType)] ->
  CHRequest ->
  Either ClickhouseSettingsException [(ByteString, ByteString)]
effectiveRequestQueryParams connectionSettings request =
  mapM render (requestQueryParams request)
 where
  occupied =
    map fst (requestParams request)
      <> map (BSC.pack . fst) connectionSettings
  render (name, value)
    | BS.null name =
        Left (ClickhouseSettingsException "Query parameter names must not be empty")
    | any (\bad -> BS.elem bad name) [0x0D, 0x0A, 0x22] =
        Left
          ( ClickhouseSettingsException
              ("Query parameter name " <> BSC.unpack name <> " must not contain CR, LF or a double quote")
          )
    | fieldName `elem` occupied =
        Left
          ( ClickhouseSettingsException
              ( "Query parameter "
                  <> BSC.unpack name
                  <> " collides with the existing parameter "
                  <> BSC.unpack fieldName
              )
          )
    | otherwise = (fieldName,) <$> renderQueryParamValue value
    where
      fieldName = "param_" <> name

-- | Render a value for the given position: @True@ when it sits inside an
-- @Array@ \/ @Tuple@ \/ @Map@, where several types need quoting.
renderValue :: Bool -> ClickhouseType -> Either ClickhouseSettingsException ByteString
renderValue nested = \case
  ClickString bytes -> pure (renderEscapedBytes nested bytes)
  ClickFixedString bytes -> pure (renderEscapedBytes nested bytes)
  ClickBool True -> pure "true"
  ClickBool False -> pure "false"
  ClickInt8 integer -> pure (renderDecimal integer)
  ClickInt16 integer -> pure (renderDecimal integer)
  ClickInt32 integer -> pure (renderDecimal integer)
  ClickInt64 integer -> pure (renderDecimal integer)
  ClickInt128 integer -> pure (renderDecimal integer)
  ClickInt256 integer -> pure (renderDecimal integer)
  ClickUInt8 integer -> pure (renderDecimal integer)
  ClickUInt16 integer -> pure (renderDecimal integer)
  ClickUInt32 integer -> pure (renderDecimal integer)
  ClickUInt64 integer -> pure (renderDecimal integer)
  ClickUInt128 integer -> pure (renderDecimal integer)
  ClickUInt256 integer -> pure (renderDecimal integer)
  ClickFloat32 float -> pure (renderFloat float)
  ClickFloat64 double -> pure (renderFloat double)
  ClickDate day -> pure (quoteWhenNested nested (renderDay day))
  ClickDate32 day -> pure (quoteWhenNested nested (renderDay day))
  ClickDateTime time -> renderDateTime time
  ClickDateTime64 precision time -> renderDateTime64 precision time
  ClickUuid uuid -> pure (quoteWhenNested nested (renderUuid uuid))
  ClickIPv4 address -> pure (quoteWhenNested nested (renderIPv4 address))
  ClickIPv6 bytes -> quoteWhenNested nested <$> renderIPv6 bytes
  ClickDecimal32 _ -> decimalError
  ClickDecimal64 _ -> decimalError
  ClickDecimal128 _ -> decimalError
  ClickDecimal256 _ -> decimalError
  ClickJSON _ -> jsonError
  ClickNullable Nothing -> pure (if nested then "NULL" else "\\N")
  ClickNullable (Just value) -> renderValue nested value
  ClickArray values -> renderContainer "[" "]" (map (renderValue True) (Vector.toList values))
  ClickTuple values -> renderContainer "(" ")" (map (renderValue True) (Vector.toList values))
  ClickMap entries ->
    renderContainer
      "{"
      "}"
      [ do
          renderedKey <- renderValue True key
          renderedValue <- renderValue True value
          pure (renderedKey <> ":" <> renderedValue)
      | (key, value) <- Vector.toList entries
      ]

decimalError :: Either ClickhouseSettingsException a
decimalError =
  Left
    ( ClickhouseSettingsException
        "Decimal query parameters are not supported: ClickDecimal32/64/128/256 carry the unscaled \
        \integer while the scale is known only to the SQL placeholder type. Pass the data as an \
        \external table (see the README) or bind a scaled integer type instead."
    )

jsonError :: Either ClickhouseSettingsException a
jsonError =
  Left
    ( ClickhouseSettingsException
        "JSON query parameters are not supported: no escaped-text form for ClickJSON has been \
        \verified. Pass the data as an external table (see the README) instead."
    )

renderContainer ::
  ByteString ->
  ByteString ->
  [Either ClickhouseSettingsException ByteString] ->
  Either ClickhouseSettingsException ByteString
renderContainer open close parts = do
  rendered <- sequence parts
  pure (open <> BS.intercalate "," rendered <> close)

renderDecimal :: (Integral scalar) => scalar -> ByteString
renderDecimal = BSC.pack . show . toInteger

renderFloat :: (RealFloat scalar, Show scalar) => scalar -> ByteString
renderFloat scalar
  | isNaN scalar = "nan"
  | isInfinite scalar = if scalar < 0 then "-inf" else "inf"
  | otherwise = BSC.pack (show scalar)

-- | @Date@ \/ @Date32@ have the same text form; ClickHouse validates the range
-- of the concrete type.
renderDay :: Day -> ByteString
renderDay day = BSC.pack (formatTime defaultTimeLocale "%Y-%m-%d" day)

-- | A @DateTime@ parameter is written as epoch seconds, which is exact
-- regardless of the time zone of the SQL placeholder.  Values before the epoch
-- are outside the @DateTime@ range and are rejected by the server, so they are
-- rejected here instead.
renderDateTime :: UTCTime -> Either ClickhouseSettingsException ByteString
renderDateTime time
  | seconds < 0 =
      Left
        ( ClickhouseSettingsException
            ( "DateTime parameter is before the epoch (1970-01-01 00:00:00 UTC) and cannot be \
              \rendered: " <>
                show time
            )
        )
  | otherwise = pure (renderDecimal seconds)
 where
  seconds = round (utcTimeToPOSIXSeconds time) :: Integer

-- | A @DateTime64@ parameter is written as epoch seconds plus a fractional
-- part (e.g. @1600000000.123@).  The server reads a bare integer inside a
-- container as seconds, so the fractional form is the only representation that
-- is exact in both positions and independent of the placeholder time zone.
renderDateTime64 :: Int -> UTCTime -> Either ClickhouseSettingsException ByteString
renderDateTime64 precision time
  | precision < 0 || precision > 9 =
      Left
        ( ClickhouseSettingsException
            ("DateTime64 precision " <> show precision <> " is outside the supported 0..9 range")
        )
  | otherwise = pure (renderDecimal whole <> fractional)
 where
  scale = 10 ^ precision :: Integer
  scaled = round (utcTimeToPOSIXSeconds time * fromIntegral scale) :: Integer
  (whole, fraction) = scaled `divMod` scale
  fractional
    | precision == 0 = ""
    | otherwise = "." <> BSC.pack (padLeft precision (show fraction))

renderUuid :: UUID -> ByteString
renderUuid = BSC.pack . UUID.toString

renderIPv4 :: Word32 -> ByteString
renderIPv4 address =
  BSC.pack
    ( intercalate
        "."
        [ show ((address `shiftR` 24) .&. 0xFF)
        , show ((address `shiftR` 16) .&. 0xFF)
        , show ((address `shiftR` 8) .&. 0xFF)
        , show (address .&. 0xFF)
        ]
    )

-- | The 16 network-order bytes of @IPv6@ as a compressed hex group literal
-- (e.g. @2001:db8::1@).  The server normalises it on display.
renderIPv6 :: ByteString -> Either ClickhouseSettingsException ByteString
renderIPv6 bytes
  | BS.length bytes /= 16 =
      Left
        ( ClickhouseSettingsException
            ("ClickIPv6 requires exactly 16 bytes, got " <> show (BS.length bytes))
        )
  | otherwise = pure (BSC.pack (renderGroups groups))
 where
  groups = [group index | index <- [0 .. 7 :: Int]]
  group index =
    fromIntegral (BS.index bytes (2 * index)) * 256
      + fromIntegral (BS.index bytes (2 * index + 1)) :: Int
  renderGroups values = case longestRun of
    Nothing -> intercalate ":" (map hexGroup values)
    Just (start, runLength) ->
      intercalate ":" (map hexGroup (take start values))
        <> "::"
        <> intercalate ":" (map hexGroup (drop (start + runLength) values))
   where
    -- RFC 5952: the first longest run of at least two zero groups is elided.
    longestRun = case [run | run@(_, len) <- zeroRuns values, len >= 2] of
      [] -> Nothing
      runs -> Just (foldr1 (\left right -> if snd right > snd left then right else left) runs)
  zeroRuns values = go 0 values
   where
    go _ [] = []
    go start remaining@(value : rest)
      | value == 0 =
          let (zeros, after) = span (== 0) remaining
           in (start, length zeros) : go (start + length zeros) after
      | otherwise = go (start + 1) rest
  hexGroup value = showHex value ""

-- | Escape a string value.  At the top level the bytes are bare, inside a
-- container they are wrapped in single quotes.
renderEscapedBytes :: Bool -> ByteString -> ByteString
renderEscapedBytes nested bytes = quoted (BS.concat (map escapeByte (BS.unpack bytes)))
 where
  quoted escaped
    | nested = "'" <> escaped <> "'"
    | otherwise = escaped
  escapeByte byte = case byte of
    0x5C -> "\\\\"
    0x27 | nested -> "\\'"
    0x09 -> "\\t"
    0x0A -> "\\n"
    0x0D -> "\\r"
    0x00 -> "\\0"
    _ | byte < 0x20 || byte == 0x7F -> BSC.pack ("\\x" <> padLeft 2 (showHex byte ""))
      | otherwise -> BS.singleton byte

quoteWhenNested :: Bool -> ByteString -> ByteString
quoteWhenNested nested value
  | nested = "'" <> value <> "'"
  | otherwise = value

padLeft :: Int -> String -> String
padLeft width text
  | length text >= width = text
  | otherwise = replicate (width - length text) '0' <> text
