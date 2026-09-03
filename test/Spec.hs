{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forM_, unless)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Conduit (ConduitT, await, runConduit, yield, (.|))
import Data.List (isInfixOf)
import Data.Ratio ((%))
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.UUID (fromWords64)
import Data.Vector qualified as Vector
import Database.Clickhouse.Client.Types (ClickhouseType (..))
import Database.Clickhouse.Conversion.Binary.Decode
  ( decodeRowBinaryBuffer
  , decodeRowBinaryC
  )
import Database.Clickhouse.Conversion.Binary.Encode
  ( encodeLEB128
  , encodeRows
  , encodeValue
  )
import Database.Clickhouse.Conversion.Types
  ( ChType (..)
  , parseChType
  )
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  results <- runAll checks
  let failures = [name | (name, Left err) <- results]
      total = length results
  forM_ results $ \case
    (name, Left err) -> hPutStrLn stderr ("FAIL " <> name <> ": " <> err)
    _ -> pure ()
  putStrLn ("passed " <> show (total - length failures) <> " of " <> show total)
  unless (null failures) exitFailure

type Check = (String, IO (Either String ()))

check :: String -> Either String () -> Check
check name result = (name, pure result)

checkIO :: String -> IO (Either String ()) -> Check
checkIO name action = (name, action)

assertEq :: (Eq a, Show a) => String -> a -> a -> Either String ()
assertEq label expected actual =
  if expected == actual
    then Right ()
    else Left (label <> ": expected " <> show expected <> ", got " <> show actual)

checks :: [Check]
checks =
  [ check "type names parse to the expected schema" $ do
      mapM_
        ( \(input, expected) -> case parseChType input of
            Left err -> Left ("parse " <> show input <> ": " <> err)
            Right actual -> assertEq ("parse " <> show input) expected actual
        )
        typeNameCases
  , check "golden byte encoding of scalar values" $ do
      goldenEncode "UInt64 1" (ClickUInt64 1) "\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL"
      goldenEncode "Int32 -2" (ClickInt32 (-2)) "\254\255\255\255"
      goldenEncode "String \"a\"" (ClickString "a") "\SOHa"
      goldenEncode "nullable present" (ClickNullable (Just (ClickUInt8 5))) "\NUL\ENQ"
      goldenEncode "nullable null" (ClickNullable Nothing) "\SOH"
      goldenEncode
        "Array(UInt16) [1,2]"
        (ClickArray (Vector.fromList [ClickUInt16 1, ClickUInt16 2]))
        "\STX\SOH\NUL\STX\NUL"
      goldenEncode
        "UUID 550e8400-e29b-41d4-a716-446655440000"
        (ClickUuid (fromWords64 0x550E8400E29B41D4 0xA716446655440000))
        (BS.pack [212, 65, 155, 226, 0, 132, 14, 85, 0, 0, 68, 85, 102, 68, 22, 167])
      goldenEncode "IPv4 1.2.3.4" (ClickIPv4 0x01020304) "\EOT\ETX\STX\SOH"
      goldenEncode "Int128 -1" (ClickInt128 (-1)) (BS.replicate 16 255)
      goldenEncode "UInt256 1" (ClickUInt256 1) (BS.pack (1 : replicate 31 0))
  , check "header buffer round trip decodes to the original rows" $ do
      let buffer = makeBuffer columnNames typeNames rows
      decoded <- decodeRowBinaryBuffer buffer
      assertEq "decoded rows" (map Vector.fromList rows) decoded
  , check "truncated row is reported as an error" $ do
      let buffer = makeBuffer ["n"] ["UInt64"] [[ClickUInt64 42]]
      case decodeRowBinaryBuffer (BS.take 5 buffer) of
        Left _ -> Right ()
        Right _ -> Left "expected a truncation error, got a successful decode"
  , check "unsupported column types fail loudly" $ do
      case parseChType "Point" of
        Left err ->
          if "unsupported" `isInfixOf` err
            then Right ()
            else Left ("unexpected error: " <> err)
        Right _ -> Left "expected Point to be rejected"
  , checkIO "streaming decode across chunk boundaries matches buffer decode" $ do
      let buffer = makeBuffer columnNames typeNames rows
      streamed <- runResourceT (runConduit (sourceChunks (chunksOf 7 buffer) .| decodeRowBinaryC .| collect))
      case decodeRowBinaryBuffer buffer of
        Left err -> pure (Left ("reference decode failed: " <> err))
        Right reference ->
          pure $ do
            assertEq "streamed equals reference" reference streamed
            assertEq "streamed row count" (length rows) (length streamed)
  ]

typeNameCases :: [(ByteString, ChType)]
typeNameCases =
  [ ("Int8", ChInt8)
  , ("Int128", ChInt128)
  , ("Int256", ChInt256)
  , ("UInt64", ChUInt64)
  , ("UInt128", ChUInt128)
  , ("UInt256", ChUInt256)
  , ("Float32", ChFloat32)
  , ("Bool", ChBool)
  , ("String", ChString)
  , ("FixedString(16)", ChFixedString 16)
  , ("Date", ChDate)
  , ("Date32", ChDate32)
  , ("DateTime", ChDateTime)
  , ("DateTime64(3, 'UTC')", ChDateTime64 3)
  , ("Decimal(18, 3)", ChDecimal 18 3)
  , ("UUID", ChUuid)
  , ("IPv4", ChIPv4)
  , ("IPv6", ChIPv6)
  , ("JSON", ChJSON)
  , ("Enum8('a' = 1, 'b' = 2)", ChEnum 8)
  , ("Nullable(String)", ChNullable ChString)
  , ("LowCardinality(String)", ChLowCardinality ChString)
  , ("Array(UInt32)", ChArray ChUInt32)
  , ("Tuple(UInt8, Array(String))", ChTuple [ChUInt8, ChArray ChString])
  , ("Map(String, UInt64)", ChMap ChString ChUInt64)
  ]

columnNames :: [ByteString]
columnNames =
  [ "n"
  , "i"
  , "s"
  , "b"
  , "d"
  , "d32"
  , "dt"
  , "dt64"
  , "u"
  , "dec"
  , "nu"
  , "arr"
  , "map"
  , "tup"
  , "fx"
  , "i128"
  , "u256"
  , "ip4"
  , "ip6"
  , "dec256"
  , "js"
  ]

typeNames :: [ByteString]
typeNames =
  [ "UInt64"
  , "Int32"
  , "String"
  , "Bool"
  , "Date"
  , "Date32"
  , "DateTime"
  , "DateTime64(3)"
  , "UUID"
  , "Decimal(18, 4)"
  , "Nullable(String)"
  , "Array(UInt16)"
  , "Map(String, UInt8)"
  , "Tuple(Int8, String)"
  , "FixedString(5)"
  , "Int128"
  , "UInt256"
  , "IPv4"
  , "IPv6"
  , "Decimal(40, 2)"
  , "JSON"
  ]

rows :: [[ClickhouseType]]
rows =
  [ [ ClickUInt64 1
    , ClickInt32 (-2)
    , ClickString "hello"
    , ClickBool True
    , ClickDate (fromGregorian 2024 2 29)
    , ClickDate32 (fromGregorian 2024 3 1)
    , ClickDateTime (posixSecondsToUTCTime 1600000000)
    , ClickDateTime64 3 (posixSecondsToUTCTime (fromRational (1638543825123 % 1000)))
    , ClickUuid (fromWords64 0x550E8400E29B41D4 0xA716446655440000)
    , ClickDecimal64 1234567890
    , ClickNullable (Just (ClickString "x"))
    , ClickArray (Vector.fromList [ClickUInt16 1, ClickUInt16 2, ClickUInt16 3])
    , ClickMap (Vector.fromList [(ClickString "k", ClickUInt8 7)])
    , ClickTuple (Vector.fromList [ClickInt8 (-1), ClickString "t"])
    , ClickFixedString "hello"
    , ClickInt128 (-1)
    , ClickUInt256 1
    , ClickIPv4 0x01020304
    , ClickIPv6 (BS.pack [0x20, 0x01, 0x0D, 0xB8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1])
    , ClickDecimal256 12345
    , ClickJSON
        (Aeson.object ["s" .= ("ok" :: String), "n" .= (1 :: Int), "arr" .= ([1, 2] :: [Int])])
    ]
  , [ ClickUInt64 2
    , ClickInt32 0
    , ClickString ""
    , ClickBool False
    , ClickDate (fromGregorian 1970 1 1)
    , ClickDate32 (fromGregorian 1969 12 31)
    , ClickDateTime (posixSecondsToUTCTime 0)
    , ClickDateTime64 3 (posixSecondsToUTCTime (fromRational (0 % 1)))
    , ClickUuid (fromWords64 0 0)
    , ClickDecimal64 0
    , ClickNullable Nothing
    , ClickArray Vector.empty
    , ClickMap Vector.empty
    , ClickTuple (Vector.fromList [ClickInt8 5, ClickString ""])
    , ClickFixedString "ab\NUL\NULc"
    , ClickInt128 0
    , ClickUInt256 0
    , ClickIPv4 0
    , ClickIPv6 (BS.replicate 16 0)
    , ClickDecimal256 0
    , ClickJSON (Aeson.object [])
    ]
  ]

makeBuffer :: [ByteString] -> [ByteString] -> [[ClickhouseType]] -> ByteString
makeBuffer names typeNamesValues rowsValues =
  header <> encodeRows (map Vector.fromList rowsValues)
  where
    header =
      BSL.toStrict . toLazyByteString $
        encodeLEB128 (fromIntegral (length names))
          <> foldMap (encodeValue . ClickString) names
          <> foldMap (encodeValue . ClickString) typeNamesValues

goldenEncode :: String -> ClickhouseType -> ByteString -> Either String ()
goldenEncode label value want =
  assertEq label want (BSL.toStrict (toLazyByteString (encodeValue value)))

chunksOf :: Int -> ByteString -> [ByteString]
chunksOf size bytes
  | BS.null bytes = []
  | otherwise =
      let (head', tail') = BS.splitAt size bytes
       in head' : chunksOf size tail'

sourceChunks :: Monad m => [ByteString] -> ConduitT i ByteString m ()
sourceChunks = mapM_ yield

collect :: Monad m => ConduitT a o m [a]
collect = go []
  where
    go acc = await >>= \case
      Nothing -> pure (reverse acc)
      Just value -> go (value : acc)

runAll :: [Check] -> IO [(String, Either String ())]
runAll = mapM (\(name, action) -> (name,) <$> action)
