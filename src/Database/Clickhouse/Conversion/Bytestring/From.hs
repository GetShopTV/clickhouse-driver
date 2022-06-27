{-# LANGUAGE FlexibleInstances #-}

module Database.Clickhouse.Conversion.Bytestring.From where

import Data.ByteString
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.Time
import Data.Time.Zones.All (tzByName)
import Data.UUID
import Database.Clickhouse.Types
import GHC.Int
import GHC.Word

data TypeValuePairBS = TypeValuePairBS
  { strType :: !ByteString,
    strValue :: !ByteString
  }

bsDropEnd :: Int -> ByteString -> ByteString
bsDropEnd n xs = BS.take (BS.length xs - n) xs

readNullable :: ByteString -> ByteString -> ClickhouseType
readNullable spec bs = if bs == "\\N" then ClickNull else converter bs
  where
    -- Remove beginning "Nullable(" and trailing ")"
    cktype = BS.drop 9 . bsDropEnd 1 $ spec
    converter = bsToClickhouseType cktype

readString :: ByteString -> ClickhouseType
readString = ClickString -- . unescape

readLowCardinalityString :: ByteString -> ClickhouseType
readLowCardinalityString = ClickString

-- Now handled at preprocessing
{- unescape :: ByteString -> ByteString
unescape = streamEdit escapeableChars escaper
  where
    escapeableChars = asum [string "\\\\", string "\\\'"]
    escaper c
      | c == "\\\\" = "\\"
      | c == "\\\'" = "\'"
      | otherwise = c -}

readUUID :: ByteString -> ClickhouseType
readUUID bs = ClickUUID . read @UUID $ C8.unpack bs

-- TODO: Better handling of errors
readDate :: ByteString -> ClickhouseType
readDate bs = ClickDate $ parseTimeOrError True defaultTimeLocale format $ C8.unpack bs
  where
    -- like "2021-12-17"
    format = "%Y-%0m-%0d"

readDateTime :: ByteString -> ByteString -> ClickhouseType
readDateTime spec = readDateTimeUTC
  where
    -- Extract inner time zone from string like DateTime(\'Europe/Moscow\')
    _innerSpec = BS.drop 11 . bsDropEnd 3 $ spec
    _timeZone = tzByName _innerSpec

-- TODO: Handle timezone correctly
readDateTimeUTC :: ByteString -> ClickhouseType
readDateTimeUTC bs = ClickDateTime $ parseTimeOrError @UTCTime True defaultTimeLocale format $ C8.unpack bs
  where
    -- like "2021-12-03 15:03:45.302880179"
    format = "%Y-%0m-%0d %H:%M:%S%Q"

readIntColumn :: ByteString -> ByteString -> ClickhouseType
readIntColumn "Int8" bs = ClickInt8 . read @Int8 $ C8.unpack bs
readIntColumn "Int16" bs = ClickInt16 . read @Int16 $ C8.unpack bs
readIntColumn "Int32" bs = ClickInt32 . read @Int32 $ C8.unpack bs
readIntColumn "Int64" bs = ClickInt64 . read @Int64 $ C8.unpack bs
readIntColumn "UInt8" bs = ClickUInt8 . read @Word8 $ C8.unpack bs
readIntColumn "UInt16" bs = ClickUInt16 . read @Word16 $ C8.unpack bs
readIntColumn "UInt32" bs = ClickUInt32 . read @Word32 $ C8.unpack bs
readIntColumn "UInt64" bs = ClickUInt64 . read @Word64 $ C8.unpack bs
readIntColumn spec bs = error ("expect an integer but got: " ++ show bs ++ " with type " ++ show spec)

readFloatColumn :: ByteString -> ByteString -> ClickhouseType
readFloatColumn "Float32" bs = ClickFloat32 . read @Float $ C8.unpack bs
readFloatColumn "Float64" bs = ClickFloat64 . read @Double $ C8.unpack bs
readFloatColumn spec bs = error ("expect an integer but got: " ++ show bs ++ " with type " ++ show spec)

-- TODO: Cover conversion of all types
bsToClickhouseType :: ByteString -> ByteString -> ClickhouseType
bsToClickhouseType chType
  | "DateTime" `isPrefixOf` chType = readDateTime chType
  | "Nullable" `isPrefixOf` chType = readNullable chType
  | "String" `isPrefixOf` chType = readString
  | "Date" `isPrefixOf` chType = readDate
  | "Int" `isPrefixOf` chType = readIntColumn chType
  | "UInt" `isPrefixOf` chType = readIntColumn chType
  | "UUID" `isPrefixOf` chType = readUUID
  | "Float" `isPrefixOf` chType = readFloatColumn chType
  | "LowCardinality(String)" `isPrefixOf` chType = readLowCardinalityString
  | otherwise = error ("Unknown Type (please implement conversion from bytestring to ClickhouseType): " ++ C8.unpack chType)
