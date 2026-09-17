{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent.MVar (newEmptyMVar, takeMVar)
import Control.Exception (evaluate, try)
import Control.Monad (forM_, unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Conduit (runConduit, yield, (.|))
import Data.Conduit.Combinators (sinkList)
import Data.Conduit.Combinators qualified as ConduitC
import Data.Conduit.List (sourceList)
import Data.List (isInfixOf)
import Data.Ratio ((%))
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.UUID (fromWords64)
import Data.Vector qualified as Vector
import Database.Clickhouse.Client.Types
  ( CHRequest (..), ClickhouseDecodeException (..), ClickhouseType (..), ClickhouseSettingsException (..)
  , commandRequest, defaultConnection, effectiveRequestParams, externalSelectRequest, externalTable
  , insertRequest, selectRequest, selectRequestWithParams, settingEnabled
  )
import Database.Clickhouse.Client.HTTP.Client (buildRequest, newHTTPTransport)
import Database.Clickhouse.Client.HTTP.Types (defaultHTTPSettings)
import Database.Clickhouse.Conversion.Binary.Decode
  ( decodeRowBinaryBuffer
  , decodeRowBinaryC
  , decodeRowBinaryBufferWithSettings
  , decodeRowBinaryCWithSettings
  , decodeRowsFromSchema
  )
import Database.Clickhouse.Conversion.Binary.Encode
  ( encodeLEB128
  , encodeRowsWithSettings
  , encodeValue
  )
import Database.Clickhouse.Conversion.Text.Escaped
  ( effectiveRequestQueryParams
  , renderQueryParamValue
  )
import Database.Clickhouse.Conversion.Types
  ( ChType (..)
  , parseChType
  , externalTypeContainsJSON
  )
import HCurl.Request qualified as Curl
import HCurl.Types qualified as CurlTypes
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import System.Timeout (timeout)

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
  settingsChecks <>
  queryParamChecks <>
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
      decoded <- decodeRowBinaryBufferWithSettings outputJSON buffer
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
  , checkIO "header-only streaming result decodes to zero rows" $ do
      let headerBytes = makeBuffer ["n"] ["UInt64"] []
      outcome <-
        try
          (runResourceT (runConduit (sourceList [headerBytes] .| decodeRowBinaryC .| sinkList)))
          :: IO (Either ClickhouseDecodeException [Vector.Vector ClickhouseType])
      pure $ case outcome of
        Left err -> Left (show err)
        Right decoded -> assertEq "decoded rows" [] decoded
  , checkIO "header-only streaming result tolerates split and empty chunks" $ do
      let headerBytes = makeBuffer ["n"] ["UInt64"] []
          chunks = BS.empty : concatMap (\chunk -> [chunk, BS.empty]) (chunksOf 1 headerBytes)
      outcome <-
        try
          (runResourceT (runConduit (sourceList chunks .| decodeRowBinaryC .| sinkList)))
          :: IO (Either ClickhouseDecodeException [Vector.Vector ClickhouseType])
      pure $ case outcome of
        Left err -> Left (show err)
        Right decoded -> assertEq "decoded rows" [] decoded
  , checkIO "partial first row still fails after split and empty chunks" $ do
      let buffer = makeBuffer ["n"] ["UInt64"] [[ClickUInt64 42]]
          truncated = BS.init buffer
          chunks = BS.empty : concatMap (\chunk -> [chunk, BS.empty]) (chunksOf 1 truncated)
      outcome <-
        try
          (runResourceT (runConduit (sourceList chunks .| decodeRowBinaryC .| sinkList)))
          :: IO (Either ClickhouseDecodeException [Vector.Vector ClickhouseType])
      pure $ case outcome of
        Left (ClickhouseDecodeException message) ->
          assertEq "decode error" "unexpected end of input in the middle of a row" message
        Right decoded -> Left ("expected a decode exception, got " <> show decoded)
  , checkIO "streaming decode across chunk boundaries matches buffer decode" $ do
      let buffer = makeBuffer columnNames typeNames rows
      streamed <- runResourceT (runConduit (sourceList (chunksOf 7 buffer) .| decodeRowBinaryCWithSettings outputJSON .| sinkList))
      case decodeRowBinaryBufferWithSettings outputJSON buffer of
        Left err -> pure (Left ("reference decode failed: " <> err))
        Right reference ->
          pure $ do
            assertEq "streamed equals reference" reference streamed
            assertEq "streamed row count" (length rows) (length streamed)
  , checkIO "streaming decode matches buffer decode at every chunk boundary" $ do
      let buffer = makeBuffer columnNames typeNames rows
          sizes = [1, 2, 3, 5, 7, 13, 64, 257]
      results <-
        mapM
          ( \size -> do
              decoded <-
                runResourceT
                  (runConduit (sourceList (chunksOf size buffer) .| decodeRowBinaryCWithSettings outputJSON .| sinkList))
              pure (size, decoded)
          )
          sizes
      pure $ case decodeRowBinaryBufferWithSettings outputJSON buffer of
        Left err -> Left ("reference decode failed: " <> err)
        Right reference -> do
          mapM_
            ( \(size, decoded) ->
                assertEq ("chunk size " <> show size) reference decoded
            )
            results
          assertEq "checked chunk size count" (length sizes) (length results)
  , checkIO "truncated streams fail loudly instead of dropping rows" $ do
      let buffer = makeBuffer ["n"] ["UInt64"] [[ClickUInt64 1], [ClickUInt64 2], [ClickUInt64 3]]
          truncated = BS.take (BS.length buffer - 3) buffer
      outcome <-
        try
          ( runResourceT
              (runConduit (sourceList (chunksOf 5 truncated) .| decodeRowBinaryC .| sinkList))
          )
          :: IO (Either ClickhouseDecodeException [Vector.Vector ClickhouseType])
      pure $ case outcome of
        Right decoded ->
          Left ("expected a decode exception, got " <> show (length decoded) <> " rows")
        Left (ClickhouseDecodeException message)
          | "end of input" `isInfixOf` message -> Right ()
          | "truncated" `isInfixOf` message -> Right ()
          | otherwise -> Left ("unexpected decode error: " <> message)
  , checkIO "rows are decoded before the response stream ends" $ do
      never <- newEmptyMVar
      let headerBytes =
            BSL.toStrict . toLazyByteString $
              encodeLEB128 1
                <> encodeValue (ClickString "n")
                <> encodeValue (ClickString "UInt64")
          rowBytes value = BSL.toStrict (toLazyByteString (encodeValue (ClickUInt64 value)))
          source = do
            yield (headerBytes <> rowBytes 1)
            _ <- liftIO (takeMVar never)
            yield (rowBytes 2)
      outcome <-
        timeout
          10000000
          (runResourceT (runConduit (source .| decodeRowBinaryC .| (ConduitC.take 1 .| sinkList))))
      pure $ case outcome of
        Nothing ->
          Left
            "decoder did not yield the first row before the stream ended (full buffering)"
        Just [decoded] -> assertEq "decoded row" (Vector.fromList [ClickUInt64 1]) decoded
        Just other -> Left ("unexpected decoded rows: " <> show other)
  ]

queryParamChecks :: [Check]
queryParamChecks =
  [ check "query parameter values render as escaped text" $
      mapM_
        ( \(label, value, expected) -> case renderQueryParamValue value of
            Left err -> Left (label <> ": unexpected renderer failure: " <> show err)
            Right rendered -> assertEq label expected rendered
        )
        queryParamGolden
  , check "unrenderable query parameters fail loudly" $
      mapM_
        ( \(label, value, fragment) -> case renderQueryParamValue value of
            Left (ClickhouseSettingsException message)
              | fragment `isInfixOf` message -> Right ()
              | otherwise -> Left (label <> ": unexpected message: " <> message)
            Right rendered -> Left (label <> ": expected a failure, rendered " <> show rendered)
        )
        queryParamFailures
  , check "typed query parameter bindings are validated and rendered in order" $ do
      let request =
            selectRequestWithParams
              [("p", ClickUInt64 3), ("s", ClickString "a\tb")]
              "SELECT {p:UInt64}"
      bindings <- either (Left . show) Right (effectiveRequestQueryParams [] request)
      assertEq "bindings" [("param_p", "3"), ("param_s", "a\\tb")] bindings
      expectBindingError
        "empty name"
        "must not be empty"
        (effectiveRequestQueryParams [] (selectRequestWithParams [("", ClickUInt64 1)] "SELECT 1"))
      expectBindingError
        "raw parameter collision"
        "param_p"
        ( effectiveRequestQueryParams
            []
            ( (selectRequestWithParams [("p", ClickUInt64 1)] "SELECT 1")
                { requestParams = [("param_p", "2")]
                }
            )
        )
      expectBindingError
        "settings collision"
        "param_p"
        (effectiveRequestQueryParams [("param_p", ClickString "")] (selectRequestWithParams [("p", ClickUInt64 1)] "SELECT 1"))
      expectBindingError
        "multipart header corruption"
        "must not contain"
        (effectiveRequestQueryParams [] (selectRequestWithParams [("p\"\r\nx", ClickUInt64 1)] "SELECT 1"))
  , checkIO "typed parameters travel as multipart param_ fields in the pinned order" $ do
      transport <- newHTTPTransport defaultHTTPSettings
      let conn = defaultConnection transport
          request =
            selectRequestWithParams
              [("p", ClickUInt64 3), ("s", ClickString "a\tb")]
              "SELECT {p:UInt64}"
          external = externalTable "ids" [("value", "UInt64")] [[ClickUInt64 1]]
      built <- buildRequest conn request
      withExternal <- buildRequest conn (request {requestExternals = [external]})
      pure $ do
        payload <- requestBodyBuffer built
        assertEq "field order" ["query", "param_p", "param_s"] (multipartFieldNames payload)
        assertEq "query field content" True ("SELECT {p:UInt64}" `BS.isInfixOf` payload)
        assertEq "typed value is escaped" True ("a\\tb" `BS.isInfixOf` payload)
        assertEq "no raw tab byte" False (BS.elem 0x09 payload)
        assertEq "multipart content type" True (any ("multipart/form-data" `BS.isInfixOf`) (requestHeaders built))
        assertEq "typed parameters stay out of the URL" False ("param_p" `BS.isInfixOf` Curl.url built)
        externalPayload <- requestBodyBuffer withExternal
        assertEq
          "external table field order"
          ["query", "param_p", "param_s", "ids_format", "ids_structure", "ids"]
          (multipartFieldNames externalPayload)
        assertEq "external tree is RowBinary" True ("RowBinary" `BS.isInfixOf` externalPayload)
  , checkIO "an INSERT with typed parameters falls back to URL parameters" $ do
      transport <- newHTTPTransport defaultHTTPSettings
      let insert =
            (insertRequest "INSERT INTO t FORMAT RowBinary" "payload")
              { requestQueryParams = [("p", ClickUInt64 3)]
              }
      built <- buildRequest (defaultConnection transport) insert
      pure $ do
        assertEq "raw INSERT body" (Right "payload") (requestBodyBuffer built)
        assertEq "typed parameter in the URL" True ("param_p=3" `BS.isInfixOf` Curl.url built)
        assertEq "SQL in the URL" True ("query=INSERT" `BS.isInfixOf` Curl.url built)
        assertEq
          "no multipart body"
          False
          (any ("multipart/form-data" `BS.isInfixOf`) (requestHeaders built))
  , checkIO "unrenderable typed parameters fail before the request is built" $ do
      transport <- newHTTPTransport defaultHTTPSettings
      outcome <-
        try
          ( buildRequest
              (defaultConnection transport)
              (selectRequestWithParams [("d", ClickDecimal32 1)] "SELECT {d:Decimal32(2)}")
          )
          :: IO (Either ClickhouseSettingsException Curl.Request)
      pure $ case outcome of
        Left (ClickhouseSettingsException message)
          | "Decimal query parameters are not supported" `isInfixOf` message -> Right ()
          | otherwise -> Left ("unexpected error: " <> message)
        Right _ -> Left "a Decimal parameter reached the HTTP request builder"
  ]
 where
  expectBindingError label fragment outcome = case outcome of
    Left (ClickhouseSettingsException message)
      | fragment `isInfixOf` message -> Right ()
      | otherwise -> Left (label <> ": unexpected message: " <> message)
    Right bindings -> Left (label <> ": expected a failure, got " <> show bindings)

queryParamGolden :: [(String, ClickhouseType, ByteString)]
queryParamGolden =
  [ ("bare string", ClickString "plain", "plain")
  , ("empty string", ClickString "", "")
  , ("backslash", ClickString "a\\b", "a\\\\b")
  , ("tab", ClickString "a\tb", "a\\tb")
  , ("newline", ClickString "a\nb", "a\\nb")
  , ("carriage return", ClickString "a\rb", "a\\rb")
  , ("NUL", ClickString "a\0b", "a\\0b")
  , ("control bytes", ClickString (BS.pack [0x01, 0x1F, 0x7F]), "\\x01\\x1f\\x7f")
  , ("bytes >= 0x80 pass through", ClickString (BS.pack [0x68, 0xC3, 0xA9, 0xFF]), BS.pack [0x68, 0xC3, 0xA9, 0xFF])
  , ("quotes stay literal at the top level", ClickString "it's \"q\"", "it's \"q\"")
  , ("FixedString shares the escaped form", ClickFixedString "ab", "ab")
  , ("Nullable null", ClickNullable Nothing, "\\N")
  , ("Nullable present", ClickNullable (Just (ClickUInt8 5)), "5")
  , ("Nullable string stays bare at the top level", ClickNullable (Just (ClickString "x")), "x")
  , ("bool true", ClickBool True, "true")
  , ("bool false", ClickBool False, "false")
  , ("negative Int32", ClickInt32 (-42), "-42")
  , ("UInt64 maxBound", ClickUInt64 maxBound, "18446744073709551615")
  , ("UInt256 maxBound", ClickUInt256 maxBound, "115792089237316195423570985008687907853269984665640564039457584007913129639935")
  , ("Float32", ClickFloat32 1.25, "1.25")
  , ("Float64", ClickFloat64 (-0.125), "-0.125")
  , ("Float64 exponent", ClickFloat64 1.0e7, "1.0e7")
  , ("Float64 nan", ClickFloat64 (0 / 0), "nan")
  , ("Float64 inf", ClickFloat64 (1 / 0), "inf")
  , ("Float64 negative inf", ClickFloat64 (-1 / 0), "-inf")
  , ("Date", ClickDate (fromGregorian 2024 2 29), "2024-02-29")
  , ("Date32", ClickDate32 (fromGregorian 1969 12 31), "1969-12-31")
  , ("DateTime as epoch seconds", ClickDateTime (posixSecondsToUTCTime 1600000000), "1600000000")
  , ("DateTime64", ClickDateTime64 3 (posixSecondsToUTCTime (fromRational (1638543825123 % 1000))), "1638543825.123")
  , ("DateTime64 zero precision", ClickDateTime64 0 (posixSecondsToUTCTime 1600000000), "1600000000")
  , ("DateTime64 pads its fraction", ClickDateTime64 9 (posixSecondsToUTCTime 1600000000), "1600000000.000000000")
  , ("DateTime64 negative fraction", ClickDateTime64 3 (posixSecondsToUTCTime (fromRational ((-315619199500) % 1000))), "-315619200.500")
  , ("UUID", ClickUuid (fromWords64 0x550E8400E29B41D4 0xA716446655440000), "550e8400-e29b-41d4-a716-446655440000")
  , ("IPv4", ClickIPv4 0x01020304, "1.2.3.4")
  , ("IPv6", ClickIPv6 ipv6Example, "2001:db8::1")
  , ("IPv6 loopback", ClickIPv6 (BS.replicate 15 0 <> BS.singleton 1), "::1")
  , ("IPv6 unspecified", ClickIPv6 (BS.replicate 16 0), "::")
  , ("IPv6 trailing zero run", ClickIPv6 (BS.pack [0x20, 0x01, 0x0D, 0xB8] <> BS.replicate 12 0), "2001:db8::")
  , ("IPv6 mapped address", ClickIPv6 (BS.replicate 10 0 <> BS.pack [0xFF, 0xFF, 1, 2, 3, 4]), "::ffff:102:304")
  , ("empty array", ClickArray Vector.empty, "[]")
  , ("array of ints", ClickArray (Vector.fromList [ClickUInt16 1, ClickUInt16 2]), "[1,2]")
  , ("array of strings is quoted", ClickArray (Vector.fromList [ClickString "it's", ClickString "x\\y"]), "['it\\'s','x\\\\y']")
  , ("nested empty array", ClickArray (Vector.singleton (ClickArray Vector.empty)), "[[]]")
  , ("nullable inside an array", ClickArray (Vector.fromList [ClickNullable Nothing, ClickNullable (Just (ClickString "x"))]), "[NULL,'x']")
  , ("array of dates is quoted", ClickArray (Vector.singleton (ClickDate (fromGregorian 2024 2 29))), "['2024-02-29']")
  , ("array of DateTime uses epoch seconds", ClickArray (Vector.fromList [ClickDateTime (posixSecondsToUTCTime 0), ClickDateTime (posixSecondsToUTCTime 1600000000)]), "[0,1600000000]")
  , ("array of UUIDs is quoted", ClickArray (Vector.singleton (ClickUuid (fromWords64 0x550E8400E29B41D4 0xA716446655440000))), "['550e8400-e29b-41d4-a716-446655440000']")
  , ("array of IPv4 is quoted", ClickArray (Vector.singleton (ClickIPv4 0x01020304)), "['1.2.3.4']")
  , ("array of IPv6 is quoted", ClickArray (Vector.singleton (ClickIPv6 ipv6Example)), "['2001:db8::1']")
  , ("tuple", ClickTuple (Vector.fromList [ClickUInt8 1, ClickString "x"]), "(1,'x')")
  , ("map", ClickMap (Vector.fromList [(ClickString "k", ClickUInt64 1)]), "{'k':1}")
  , ( "nested containers"
    , ClickTuple
        ( Vector.fromList
            [ ClickMap (Vector.fromList [(ClickString "k", ClickArray (Vector.fromList [ClickUInt8 1]))])
            , ClickString "z"
            ]
        )
    , "({'k':[1]},'z')"
    )
  ]
 where
  ipv6Example = BS.pack [0x20, 0x01, 0x0D, 0xB8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1]

queryParamFailures :: [(String, ClickhouseType, String)]
queryParamFailures =
  [ ("Decimal32", ClickDecimal32 123, "Decimal query parameters are not supported")
  , ("Decimal64", ClickDecimal64 123, "Decimal query parameters are not supported")
  , ("Decimal128", ClickDecimal128 123, "Decimal query parameters are not supported")
  , ("Decimal256", ClickDecimal256 123, "Decimal query parameters are not supported")
  , ("JSON", ClickJSON (Aeson.object []), "JSON query parameters are not supported")
  , ("pre-epoch DateTime", ClickDateTime (posixSecondsToUTCTime (-1)), "before the epoch")
  , ("DateTime64 precision", ClickDateTime64 10 (posixSecondsToUTCTime 0), "precision")
  , ("IPv6 length", ClickIPv6 (BS.pack [1, 2, 3]), "16 bytes")
  ]

-- | The @name@ attribute of every multipart part, in body order.
multipartFieldNames :: ByteString -> [ByteString]
multipartFieldNames = go
 where
  needle = "form-data; name=\""
  go bytes = case BS.breakSubstring needle bytes of
    (_, rest)
      | BS.null rest -> []
      | otherwise ->
          let afterNeedle = BS.drop (BS.length needle) rest
              (fieldName, remaining) = BS.breakSubstring "\"" afterNeedle
           in fieldName : go remaining

requestBodyBuffer :: Curl.Request -> Either String ByteString
requestBodyBuffer request = case Curl.body request of
  CurlTypes.Buffer payload -> Right payload
  CurlTypes.Empty -> Left "expected a buffered request body"

requestHeaders :: Curl.Request -> [ByteString]
requestHeaders request = case Curl.headers request of
  Curl.HeaderList headers -> headers
  _ -> []

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
  , ("DateTime('UTC')", ChDateTime)
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
  header <> encodeRowsWithSettings inputJSON (map Vector.fromList rowsValues)
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

runAll :: [Check] -> IO [(String, Either String ())]
runAll = mapM (\(name, action) -> (name,) <$> action)

outputJSON :: [(ByteString, ByteString)]
outputJSON = [("output_format_binary_write_json_as_string", "1")]

inputJSON :: [(ByteString, ByteString)]
inputJSON = [("input_format_binary_read_json_as_string", "1")]

settingsChecks :: [Check]
settingsChecks =
  [ check "external JSON detection does not require a supported response decoder" $
      mapM_
        (\typeName -> externalTypeContainsJSON typeName >>= assertEq (show typeName) False)
        [ "DateTime('UTC')"
        , "Decimal32(2)"
        , "SimpleAggregateFunction(sum, UInt64)"
        , "Tuple(JSON UInt64, label String)"
        , "Tuple(`JSON` UInt64, \"label\" String)"
        , "Enum8('JSON' = -1, 'other' = 1)"
        , "Enum8('it\\'s JSON' = -1, 'it''s JSON' = 1)"
        , "Array(Tuple(label String, timestamp DateTime('UTC')))"
        ]
  , check "external JSON detection traverses named fields and unknown wrappers" $
      mapM_
        (\typeName -> externalTypeContainsJSON typeName >>= assertEq (show typeName) True)
        [ "JSON"
        , "json"
        , "JSON(max_dynamic_paths=16)"
        , "Nullable(JSON)"
        , "LowCardinality(JSON)"
        , "Array(Tuple(value JSON))"
        , "Tuple(JSON UInt64, value Array(JSON))"
        , "Map(String, JSON)"
        , "FutureWrapper(Tuple(`value` JSON))"
        , "Array(Tuple(value jSoN))"
        ]
  , check "malformed external schemas fail closed" $
      mapM_
        ( \typeName -> case externalTypeContainsJSON typeName of
            Left _ -> Right ()
            Right _ -> Left ("accepted malformed schema: " <> show typeName)
        )
        ["Array(JSON", "Tuple(value JSON))", "Array(JSON) trailing", "Enum8('JSON = 1)"]
  , check "request builders send no implicit settings" $ do
      let builders =
            [ selectRequest "SELECT 1"
            , externalSelectRequest [] "SELECT 1"
            , insertRequest "INSERT INTO t FORMAT RowBinary" ""
            , commandRequest "SELECT 1"
            ]
      mapM_ (assertEq "params" [] . requestParams) builders
      mapM_ (assertEq "typed params" [] . requestQueryParams) builders
      assertEq
        "parameterized select"
        [("p", ClickUInt64 3)]
        (requestQueryParams (selectRequestWithParams [("p", ClickUInt64 3)] "SELECT 1"))
  , check "typed settings render scalar values exactly" $ do
      let values =
            [ ("bool", ClickBool True)
            , ("false", ClickBool False)
            , ("string", ClickString "a &+%?#=\NUL\255")
            , ("int", ClickInt64 (-42))
            , ("uint", ClickUInt64 maxBound)
            , ("wide", ClickUInt256 maxBound)
            , ("float32", ClickFloat32 1.25)
            , ("float64", ClickFloat64 (-0.125))
            ]
      params <- either (Left . show) Right (effectiveRequestParams values (selectRequest "SELECT 1"))
      assertEq "rendered" [("bool", "1"), ("false", "0"), ("string", "a &+%?#=\NUL\255"), ("int", "-42"), ("uint", "18446744073709551615"), ("wide", "115792089237316195423570985008687907853269984665640564039457584007913129639935"), ("float32", "1.25"), ("float64", "-0.125")] params
  , check "unsupported and nonfinite settings are explicit errors" $
      mapM_
        ( \value -> case effectiveRequestParams [("bad_setting", value)] (selectRequest "SELECT 1") of
            Left (ClickhouseSettingsException message) | "bad_setting" `isInfixOf` message && "finite Float" `isInfixOf` message -> Right ()
            other -> Left ("unexpected result: " <> show other)
        )
        [ClickArray Vector.empty, ClickTuple Vector.empty, ClickMap Vector.empty, ClickNullable Nothing, ClickJSON (Aeson.object []), ClickFloat32 (0 / 0), ClickFloat64 (1 / 0), ClickFloat64 (-1 / 0)]
  , check "settings cannot replace SQL parameters or connection identity" $
      mapM_
        ( \name -> case effectiveRequestParams [(name, ClickString "wrong")] (selectRequest "SELECT 1") of
            Left _ -> Right ()
            Right _ -> Left ("accepted reserved setting " <> name)
        )
        ["param_name", "query", "user", "password", "database", "format", "default_format"]
  , check "request parameters take precedence without duplicates or altered SQL bindings" $ do
      let request = (selectRequest "SELECT {value:String}"){requestParams = [("output_format_binary_write_json_as_string", "0"), ("output_format_binary_write_json_as_string", "1"), ("param_value", "a&+%")]}
      params <- either (Left . show) Right (effectiveRequestParams [("output_format_binary_write_json_as_string", ClickBool True), ("max_threads", ClickInt8 2), ("max_threads", ClickInt8 3)] request)
      assertEq "effective params" [("output_format_binary_write_json_as_string", "0"), ("param_value", "a&+%"), ("max_threads", "2")] params
      assertEq "output disabled" False (settingEnabled "output_format_binary_write_json_as_string" params)
  , check "typed JSON mode follows exact server boolean representations" $
      mapM_
        ( \(value, enabled) -> do
            params <- either (Left . show) Right (effectiveRequestParams [("output_format_binary_write_json_as_string", value)] (selectRequest "SELECT 1"))
            assertEq "mode" enabled (settingEnabled "output_format_binary_write_json_as_string" params)
        )
        [(ClickBool True, True), (ClickBool False, False), (ClickUInt8 1, True), (ClickInt8 0, False), (ClickString "TrUe", True), (ClickString "FALSE", False), (ClickUInt8 2, False)]
  , check "JSON schema rejects missing or disabled output mode even with zero rows" $
      mapM_
        ( \typeName ->
            mapM_
              ( \params -> case decodeRowBinaryBufferWithSettings params (makeBuffer ["json"] [typeName] []) of
                  Left message | "output_format_binary_write_json_as_string=1" `isInfixOf` message -> Right ()
                  other -> Left ("unexpected result: " <> show other)
              )
              [[], inputJSON, [("output_format_binary_write_json_as_string", "0")]]
        )
        ["JSON", "Nullable(JSON)", "LowCardinality(JSON)", "Array(JSON)", "Tuple(String, Array(JSON))", "Map(String, JSON)", "Map(JSON, String)"]
  , check "low level schema decoder defaults safely" $ case decodeRowsFromSchema [ChArray ChJSON] "" of
      Left message | "output_format_binary_write_json_as_string=1" `isInfixOf` message -> Right ()
      other -> Left ("unexpected result: " <> show other)
  , checkIO "JSON stream rejects schema before awaiting any row bytes" $ do
      never <- newEmptyMVar
      let source = yield (makeBuffer ["json"] ["Array(JSON)"] []) >> liftIO (takeMVar never)
      outcome <- timeout 1000000 (try (runResourceT (runConduit (source .| decodeRowBinaryC .| sinkList))) :: IO (Either ClickhouseDecodeException [Vector.Vector ClickhouseType]))
      pure $ case outcome of
        Just (Left (ClickhouseDecodeException message)) | "output_format_binary_write_json_as_string=1" `isInfixOf` message -> Right ()
        _ -> Left "JSON schema did not fail promptly with an actionable error"
  , checkIO "JSON input needs its own flag, including nested values" $ do
      results <- mapM (\params -> try (evaluate (BS.length (encodeRowsWithSettings params [Vector.singleton (ClickArray (Vector.singleton (ClickJSON (Aeson.object []))))]))) :: IO (Either ClickhouseSettingsException Int)) [[], outputJSON]
      pure $
        mapM_
          ( \result -> case result of
              Left (ClickhouseSettingsException message) | "input_format_binary_read_json_as_string=1" `isInfixOf` message -> Right ()
              _ -> Left "JSON encoded without the input flag"
          )
          results
  , check "explicit nested JSON output decodes, including empty result" $ do
      let value = ClickTuple (Vector.fromList [ClickArray (Vector.singleton (ClickJSON (Aeson.object ["n" .= (1 :: Int)])))])
      decoded <- decodeRowBinaryBufferWithSettings outputJSON (makeBuffer ["json"] ["Tuple(Array(JSON))"] [[value]])
      assertEq "nested value" [Vector.singleton value] decoded
      empty <- decodeRowBinaryBufferWithSettings outputJSON (makeBuffer ["json"] ["Tuple(Array(JSON))"] [])
      assertEq "empty JSON" [] empty
  ]
