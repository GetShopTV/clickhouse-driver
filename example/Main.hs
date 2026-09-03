{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Integration harness against a live ClickHouse server.

Configuration is read from the environment (all optional):

  * @CH_URL@      scheme + host, default @http:\/\/localhost@
  * @CH_PORT@     port, default @8123@
  * @CH_DATABASE@ database, default @default@
  * @CH_USER@     user, default @default@
  * @CH_PASSWORD@ password, default empty

The run prints a human readable transcript.  Every verification failure
aborts with a non-zero exit code.
-}
module Main (main) where

import Control.Exception (SomeException, displayException, try)
import Control.Monad (forM_, unless)
import Data.Bits (shiftR, (.&.))
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as ByteString
import Data.ByteString.Lazy qualified as BSL
import Data.List (intercalate)
import Data.Maybe (isJust)
import Data.Ratio ((%))
import Data.Text qualified as Text
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.UUID (fromWords64)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Word (Word32)
import Database.ClickHouse
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

settingsFromEnv :: IO (ClickhouseConnectionSettings ClientHTTP)
settingsFromEnv = do
  url <- maybe "http://localhost" id <$> lookupEnv "CH_URL"
  rawPort <- lookupEnv "CH_PORT"
  databaseText <- maybe "default" id <$> lookupEnv "CH_DATABASE"
  user <- maybe "default" id <$> lookupEnv "CH_USER"
  password <- maybe "" id <$> lookupEnv "CH_PASSWORD"
  let port = maybe 8123 read rawPort
  transport <-
    newHTTPTransport
      ( defaultHTTPSettings
          { clickhouseUrl = ByteString.pack url
          , port
          }
      )
  pure $
    (defaultConnection transport)
      { username = Text.pack user
      , password = Text.pack password
      , database = Text.pack databaseText
      }

main :: IO ()
main = do
  settings <- settingsFromEnv
  outcome <- try (run settings)
  case outcome of
    Left (exception :: SomeException) -> do
      hPutStrLn stderr ("integration test failed: " <> displayException exception)
      exitFailure
    Right () -> pure ()

run :: ClickhouseConnectionSettings ClientHTTP -> IO ()
run settings = do
  putStrLn "== clickhouse-driver integration run =="
  versionRows <- runQuery settings "SELECT version()"
  let version = showCell (Vector.head (Vector.head versionRows))
  putStrLn ("server version: " <> version)

  readOnly <- isJust <$> lookupEnv "CH_READONLY"
  if readOnly
    then externalProbe settings
    else do
      basic settings
      wideTypes settings
  putStrLn "integration run finished"

-- Read-only probe: no DDL, exercises multipart external tables (the pattern
-- used for binary query parameters).
externalProbe :: ClickhouseConnectionSettings ClientHTTP -> IO ()
externalProbe settings = do
  let ids = externalTable "ids" [("value", "UInt64")] [[ClickUInt64 1], [ClickUInt64 2]]
      users =
        externalTable
          "users"
          [("id", "UInt64"), ("name", "String")]
          [ [ClickUInt64 1, ClickString "alice"]
          , [ClickUInt64 2, ClickString "bob"]
          ]

  idsBack <-
    runQueryWithExternals settings [ids] "SELECT value FROM ids ORDER BY value"
  verifyEqual
    "external scalar table"
    (map Vector.fromList [[ClickUInt64 1], [ClickUInt64 2]])
    (Vector.toList idsBack)

  usersBack <-
    runQueryWithExternals
      settings
      [ids, users]
      "SELECT id, name FROM users ORDER BY id"
  verifyEqual
    "external typed table"
    ( map
        Vector.fromList
        [ [ClickUInt64 1, ClickString "alice"]
        , [ClickUInt64 2, ClickString "bob"]
        ]
    )
    (Vector.toList usersBack)

  putStrLn "ok: external tables (multipart RowBinary) round-tripped"

basic :: ClickhouseConnectionSettings ClientHTTP -> IO ()
basic settings = do
  runCommand settings "DROP TABLE IF EXISTS driver_it"
  runCommand
    settings
    "CREATE TABLE driver_it (id UInt64, name String, value Float64, flag Bool, at DateTime, tags Array(String)) ENGINE = Memory"
  putStrLn "created table driver_it"

  let basicRows =
        [ [ ClickUInt64 1
          , ClickString "one"
          , ClickFloat64 1.5
          , ClickBool True
          , ClickDateTime (posixSecondsToUTCTime 1_600_000_000)
          , ClickArray (Vector.fromList [ClickString "a", ClickString "b"])
          ]
        , [ ClickUInt64 2
          , ClickString "two"
          , ClickFloat64 2.5
          , ClickBool False
          , ClickDateTime (posixSecondsToUTCTime 1_600_000_001)
          , ClickArray (Vector.fromList [ClickString "c"])
          ]
        ]
  runInsert settings "driver_it" ["id", "name", "value", "flag", "at", "tags"] basicRows
  putStrLn "inserted 2 rows (RowBinary)"

  rows <- runQuery settings "SELECT id, name, value, flag, at, tags FROM driver_it ORDER BY id"
  putStrLn ("queried back " <> show (Vector.length rows) <> " rows")
  forM_ rows (putStrLn . renderRow)
  verifyEqual "basic round trip" (map Vector.fromList basicRows) (Vector.toList rows)
  putStrLn "ok: values round-tripped through the live server"

  streamed <- runQuery settings "SELECT id, name FROM driver_it ORDER BY id"
  unless (Vector.length streamed == 2) (fail "streaming select returned wrong row count")
  putStrLn "ok: streaming query returned rows"

  runCommand settings "DROP TABLE driver_it"
  putStrLn "cleaned up"

wideTypes :: ClickhouseConnectionSettings ClientHTTP -> IO ()
wideTypes settings = do
  runCommand settings "DROP TABLE IF EXISTS driver_it_types"
  runCommand
    settings
    "CREATE TABLE driver_it_types (id UInt64, d Date, d32 Date32, dt DateTime, dt64 DateTime64(3), u UUID, dec Decimal(18, 4), nu Nullable(String), arr Array(UInt16), mp Map(String, UInt8), tp Tuple(Int8, String), fx FixedString(5), b Bool, f Float64, i128 Int128, u256 UInt256, ip4 IPv4, ip6 IPv6, dec256 Decimal(40, 2), j JSON) ENGINE = Memory"
  putStrLn "created table driver_it_types"

  let typeRows =
        [ [ ClickUInt64 1
          , ClickDate (fromGregorian 2024 2 29)
          , ClickDate32 (fromGregorian 1969 12 31)
          , ClickDateTime (posixSecondsToUTCTime 1_600_000_000)
          , ClickDateTime64 3 (posixSecondsToUTCTime (fromRational (1_638_543_825_123 % 1000)))
          , ClickUuid (fromWords64 0x550E8400E29B41D4 0xA716446655440000)
          , ClickDecimal64 1234567890
          , ClickNullable (Just (ClickString "hi"))
          , ClickArray (Vector.fromList [ClickUInt16 1, ClickUInt16 2, ClickUInt16 3])
          , ClickMap (Vector.fromList [(ClickString "k", ClickUInt8 7)])
          , ClickTuple (Vector.fromList [ClickInt8 (-1), ClickString "t"])
          , ClickFixedString "hello"
          , ClickBool True
          , ClickFloat64 1.5
          , ClickInt128 (-1)
          , ClickUInt256 1
          , ClickIPv4 0x01020304
          , ClickIPv6 (BS.pack [0x20, 0x01, 0x0D, 0xB8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1])
          , ClickDecimal256 12345
          , ClickJSON (Aeson.object ["s" .= ("ok" :: String), "nested" .= (Aeson.object ["a" .= ("b" :: String)])])
          ]
        , [ ClickUInt64 2
          , ClickDate (fromGregorian 1970 1 1)
          , ClickDate32 (fromGregorian 2024 3 1)
          , ClickDateTime (posixSecondsToUTCTime 0)
          , ClickDateTime64 3 (posixSecondsToUTCTime (fromRational (0 % 1)))
          , ClickUuid (fromWords64 0 0)
          , ClickDecimal64 0
          , ClickNullable Nothing
          , ClickArray Vector.empty
          , ClickMap Vector.empty
          , ClickTuple (Vector.fromList [ClickInt8 5, ClickString ""])
          , ClickFixedString "ab\NUL\NULc"
          , ClickBool False
          , ClickFloat64 (-2.5)
          , ClickInt128 0
          , ClickUInt256 0
          , ClickIPv4 0
          , ClickIPv6 (BS.replicate 16 0)
          , ClickDecimal256 0
          , ClickJSON (Aeson.object [])
          ]
        ]
  runInsert
    settings
    "driver_it_types"
    ["id", "d", "d32", "dt", "dt64", "u", "dec", "nu", "arr", "mp", "tp", "fx", "b", "f", "i128", "u256", "ip4", "ip6", "dec256", "j"]
    typeRows
  putStrLn "inserted 2 wide-typed rows"

  wideRows <-
    runQuery
      settings
      "SELECT id, d, d32, dt, dt64, u, dec, nu, arr, mp, tp, fx, b, f, i128, u256, ip4, ip6, dec256, j FROM driver_it_types ORDER BY id"
  forM_ wideRows (putStrLn . renderRow)
  verifyEqual "wide type round trip" (map Vector.fromList typeRows) (Vector.toList wideRows)

  runCommand settings "DROP TABLE driver_it_types"
  putStrLn "cleaned up wide table"

verifyEqual :: String -> [Vector ClickhouseType] -> [Vector ClickhouseType] -> IO ()
verifyEqual label expected actual =
  unless (expected == actual) $
    fail (label <> ": expected " <> show expected <> ", got " <> show actual)

renderRow :: Vector ClickhouseType -> String
renderRow row =
  "  | " <> intercalate " | " (map showCell (Vector.toList row))

showCell :: ClickhouseType -> String
showCell = \case
  ClickString bs -> ByteString.unpack bs
  ClickFixedString bs -> "fx(" <> ByteString.unpack bs <> ")"
  ClickBool b -> show b
  ClickInt8 n -> show n
  ClickInt16 n -> show n
  ClickInt32 n -> show n
  ClickInt64 n -> show n
  ClickInt128 n -> show n
  ClickInt256 n -> show n
  ClickUInt8 n -> show n
  ClickUInt16 n -> show n
  ClickUInt32 n -> show n
  ClickUInt64 n -> show n
  ClickUInt128 n -> show n
  ClickUInt256 n -> show n
  ClickFloat32 f -> show f
  ClickFloat64 d -> show d
  ClickDate day -> show day
  ClickDate32 day -> show day
  ClickDateTime time -> show time
  ClickDateTime64 _ time -> show time
  ClickUuid uuid -> show uuid
  ClickIPv4 address -> showIPv4 address
  ClickIPv6 bytes -> "ip6:" <> hexBytes bytes
  ClickDecimal32 n -> show n
  ClickDecimal64 n -> show n
  ClickDecimal128 n -> show n
  ClickDecimal256 n -> show n
  ClickJSON value -> ByteString.unpack (BSL.toStrict (Aeson.encode value))
  ClickNullable Nothing -> "NULL"
  ClickNullable (Just value) -> showCell value
  ClickArray values -> "[" <> intercalate ", " (map showCell (Vector.toList values)) <> "]"
  ClickTuple values -> "(" <> intercalate ", " (map showCell (Vector.toList values)) <> ")"
  ClickMap entries ->
    "{"
      <> intercalate ", " (map (\(k, v) -> showCell k <> ": " <> showCell v) (Vector.toList entries))
      <> "}"

showIPv4 :: Word32 -> String
showIPv4 address =
  intercalate "." [show (address `shiftR` 24 .&. 0xFF), show (address `shiftR` 16 .&. 0xFF), show (address `shiftR` 8 .&. 0xFF), show (address .&. 0xFF)]

hexBytes :: ByteString -> String
hexBytes = concatMap (\b -> let (hi, lo) = (fromIntegral b `div` 16, fromIntegral b `mod` 16) in [hexDigit hi, hexDigit lo]) . BS.unpack

hexDigit :: Int -> Char
hexDigit n = "0123456789abcdef" !! n
