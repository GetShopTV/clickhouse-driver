{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Description of ClickHouse column types as they arrive in the
@RowBinaryWithNamesAndTypes@ response header, plus helpers to render the SQL
of an INSERT statement.

The type-name grammar parsed here is the one ClickHouse uses when serialising
column types as strings:

> typeName := ident | ident '(' arg (',' arg)* ')'
> arg      := number | '\'' string '\'' | typeName

Only the parts that matter for the binary layout are kept.  In particular the
timezone inside @DateTime64(3, 'UTC')@ is dropped, since the server already
normalises values to UTC before serialising them.
-}
module Database.Clickhouse.Conversion.Types
  ( ChType (..)
  , parseChType
  , decimalWidthBytes
  , renderInsertStatement
  , defaultResponseFormat
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as C8
import Data.Char (isAlphaNum, isDigit, isSpace)
import Data.Text (Text)
import Data.Text qualified as Text

-- | Decoded shape of a ClickHouse column type.
data ChType
  = ChInt8
  | ChInt16
  | ChInt32
  | ChInt64
  | ChUInt8
  | ChUInt16
  | ChUInt32
  | ChUInt64
  | ChFloat32
  | ChFloat64
  | ChBool
  | ChString
  | ChFixedString !Int
  | ChDate
  | ChDate32
  | ChDateTime
  | ChDateTime64 !Int -- ^ fractional-digit precision
  | ChDecimal !Int !Int -- ^ precision, scale
  | ChUuid
  | ChEnum !Int -- ^ underlying integer width in bits (8 or 16)
  | ChNullable !ChType
  | ChLowCardinality !ChType
  | ChArray !ChType
  | ChTuple ![ChType]
  | ChMap !ChType !ChType
  deriving stock (Show, Eq)

-- | Wire width in bytes of a @Decimal(p, s)@ value.
decimalWidthBytes :: Int -> Int
decimalWidthBytes precision
  | precision <= 9 = 4
  | precision <= 18 = 8
  | otherwise = 16

-- | Response format requested through the @X-ClickHouse-Format@ header.
defaultResponseFormat :: ByteString
defaultResponseFormat = "RowBinaryWithNamesAndTypes"

-- | Parse a ClickHouse column type name (e.g. @Nullable(Array(UInt32))@).
parseChType :: ByteString -> Either String ChType
parseChType input = do
  (t, rest) <- parseNested (C8.dropWhile isSpace input)
  if C8.all isSpace rest
    then Right t
    else Left ("trailing garbage after type: " <> show rest)

parseNested :: ByteString -> Either String (ChType, ByteString)
parseNested input = do
  (ident, rest0) <- readIdent (C8.dropWhile isSpace input)
  case C8.uncons (C8.dropWhile isSpace rest0) of
    Nothing -> pure (plainType ident, mempty)
    Just ('(', rest) -> do
      (args, rest') <- readArgs (C8.dropWhile isSpace rest)
      applied <- applyArgs ident args
      pure (applied, rest')
    Just _ -> Left ("expected '(' or end of input after " <> show ident)

data Arg
  = ANum !Int
  | AStr !ByteString
  | AType !ChType

-- | Parse a parenthesised, comma separated argument list up to the closing ')'.
readArgs :: ByteString -> Either String ([Arg], ByteString)
readArgs = go []
  where
    go acc raw = case C8.uncons (C8.dropWhile isSpace raw) of
      Just (')', rest) -> Right (reverse acc, rest)
      Just (',', rest) -> go acc rest
      Just ('\'', rest) -> do
        let (body, rest') = C8.break (== '\'') rest
        if C8.null rest'
          then Left "unterminated quoted argument"
          else go (AStr body : acc) (C8.drop 1 rest')
      Just ('(', _) -> do
        (t, rest') <- parseNested (C8.dropWhile isSpace raw)
        go (AType t : acc) rest'
      Just ('=', rest) -> do
        let (digits, restDigits) = C8.span isDigit (C8.dropWhile isSpace rest)
        if C8.null digits
          then Left "expected a number after '=' in a type argument"
          else go acc restDigits
      Just _ ->
        let bs = C8.dropWhile isSpace raw
            (digits, restDigits) = C8.span isDigit bs
         in if C8.null digits
              then do
                (ident, rest0) <- readIdent bs
                if C8.isPrefixOf "(" (C8.dropWhile isSpace rest0)
                  then do
                    (t, rest1) <- parseNested bs
                    go (AType t : acc) rest1
                  else go (AType (plainType ident) : acc) rest0
              else do
                n <- readNumber digits
                go (ANum n : acc) restDigits
      Nothing -> Left "unterminated type argument list"

applyArgs :: ByteString -> [Arg] -> Either String ChType
applyArgs name args = case name of
  "Nullable" -> ChNullable <$> singleType
  "LowCardinality" -> ChLowCardinality <$> singleType
  "Array" -> ChArray <$> singleType
  "Tuple" -> ChTuple <$> traverse asType args
  "Map" -> case args of
    [k, v] -> ChMap <$> asType k <*> asType v
    _ -> Left "Map expects two type arguments"
  "FixedString" -> ChFixedString <$> singleNum
  "Decimal" -> case args of
    [p, s] -> ChDecimal <$> asNum p <*> asNum s
    _ -> Left "Decimal expects (precision, scale)"
  "DateTime64" -> ChDateTime64 <$> firstNum
  "Enum8" -> pure (ChEnum 8)
  "Enum16" -> pure (ChEnum 16)
  _ -> Left ("unsupported parametrised ClickHouse type: " <> show name)
  where
    singleType = case args of
      [a] -> asType a
      _ -> Left ("expected exactly one type argument for " <> show name)
    singleNum = case args of
      [a] -> asNum a
      _ -> Left ("expected exactly one numeric argument for " <> show name)
    firstNum = case args of
      [] -> Left ("expected a numeric argument for " <> show name)
      a : _ -> asNum a
    asType = \case
      AType t -> Right t
      _ -> Left "expected a type argument"
    asNum = \case
      ANum n -> Right n
      _ -> Left "expected a numeric argument"

plainType :: ByteString -> ChType
plainType name = case name of
  "Int8" -> ChInt8
  "Int16" -> ChInt16
  "Int32" -> ChInt32
  "Int64" -> ChInt64
  "UInt8" -> ChUInt8
  "UInt16" -> ChUInt16
  "UInt32" -> ChUInt32
  "UInt64" -> ChUInt64
  "Float32" -> ChFloat32
  "Float64" -> ChFloat64
  "Bool" -> ChBool
  "String" -> ChString
  "Date" -> ChDate
  "Date32" -> ChDate32
  "DateTime" -> ChDateTime
  "UUID" -> ChUuid
  _ -> ChString -- unknown scalar types degrade to raw String bytes

readIdent :: ByteString -> Either String (ByteString, ByteString)
readIdent bs =
  let (ident, rest) = C8.span (\c -> isAlphaNum c || c == '_') bs
   in if C8.null ident
        then Left ("expected identifier, got " <> show (C8.take 20 bs))
        else Right (ident, rest)

readNumber :: ByteString -> Either String Int
readNumber bs =
  case reads (C8.unpack bs) of
    [(n, "")] -> Right n
    _ -> Left ("expected integer, got " <> show bs)

-- | Render @INSERT INTO table (col1, col2) FORMAT RowBinary@.
--
-- The table name is used verbatim (so qualified names like @db.table@ keep
-- working); column names are double-quoted with embedded quotes doubled.
renderInsertStatement :: Text -> [Text] -> Text
renderInsertStatement table columns =
  "INSERT INTO "
    <> table
    <> if null columns
      then " FORMAT RowBinary"
      else " (" <> Text.intercalate ", " (map quote columns) <> ") FORMAT RowBinary"
  where
    quote c = "\"" <> Text.replace "\"" "\"\"" c <> "\""
