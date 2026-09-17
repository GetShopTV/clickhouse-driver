{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Concurrent (forkIO, killThread)
import Control.Exception
  ( SomeException
  , bracket
  , displayException
  , finally
  , fromException
  , try
  )
import Control.Monad (forM_, unless, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.Char (toLower, toUpper)
import Data.List (isInfixOf)
import Data.Conduit (ConduitT, await, runConduit, (.|))
import Data.Conduit.Combinators (sinkList, sinkNull)
import Data.Conduit.Combinators qualified as ConduitC
import Data.Ratio ((%))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Unique (hashUnique, newUnique)
import Data.UUID (fromWords64)
import Data.Vector qualified as Vector
import Data.Word (Word64)
import Database.ClickHouse
import Database.Clickhouse.Conversion.Binary.Encode (encodeRowsWithSettings)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats (getRTSStats, getRTSStatsEnabled, max_live_bytes)
import Numeric (showHex)
import Network.Socket
  ( Family (AF_INET)
  , SockAddr (..)
  , Socket
  , SocketOption (..)
  , SocketType (Stream)
  , accept
  , bind
  , close
  , defaultProtocol
  , getSocketName
  , listen
  , setSocketOption
  , socket
  , tupleToHostAddress
  )
import Network.Socket.ByteString qualified as SocketBS
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import System.Timeout (timeout)

main :: IO ()
main = do
  mUrl <- lookupEnv "CH_URL"
  case mUrl of
    Nothing ->
      putStrLn
        "integration: SKIP (set CH_URL/CH_PORT/CH_USER/CH_PASSWORD to run live checks)"
    Just url -> do
      rawPort <- lookupEnv "CH_PORT"
      databaseText <- maybe "default" id <$> lookupEnv "CH_DATABASE"
      user <- maybe "default" id <$> lookupEnv "CH_USER"
      password <- maybe "" id <$> lookupEnv "CH_PASSWORD"
      allowWrites <- (== Just "1") <$> lookupEnv "CH_INTEGRATION_ALLOW_WRITES"
      let chPort = maybe 8123 read rawPort
      base <-
        connectHTTP
          defaultHTTPSettings
            { clickhouseUrl = BSC.pack url
            , port = chPort
            , connectionTimeoutMS = 5000
            }
      let conn =
            base
              { username = Text.pack user
              , password = Text.pack password
              , database = Text.pack databaseText
              }
      reachable <-
        timeout
          20000000
          (try (runQuery conn "SELECT 1") :: IO (Either SomeException (Vector.Vector (Vector.Vector ClickhouseType))))
      case reachable of
        Just (Right _) -> pure ()
        Just (Left err) -> do
          hPutStrLn
            stderr
            ( "integration: cannot reach ClickHouse at " <> url <> ":" <> show chPort
                <> ": "
                <> displayException err
            )
          exitFailure
        Nothing -> do
          hPutStrLn stderr ("integration: timed out connecting to " <> url <> ":" <> show chPort)
          exitFailure
      putStrLn
        ( "integration: server "
            <> url
            <> ":"
            <> show chPort
            <> ", database "
            <> databaseText
            <> ", writes "
            <> (if allowWrites then "enabled" else "disabled (read-only checks only)")
        )
      unless allowWrites $
        putStrLn
          "integration: DDL/DML checks are skipped unless CH_INTEGRATION_ALLOW_WRITES=1 \
          \and the target database is disposable"
      args <- getArgs
      readonlyUser <- lookupEnv "CH_READONLY_USER"
      let selected = if args == ["--settings-only"] then [] else checks conn allowWrites
      results <- runAll (settingsChecks conn readonlyUser <> selected)
      let failures = [name | (name, Left _) <- results]
      forM_ results $ \case
        (name, Left err) -> hPutStrLn stderr ("FAIL " <> name <> ": " <> err)
        (name, Right ()) -> putStrLn ("ok   " <> name)
      putStrLn
        ( "integration: passed "
            <> show (length results - length failures)
            <> " of "
            <> show (length results)
        )
      unless (null failures) exitFailure

type Check = (String, IO ())

checkIO :: String -> IO () -> Check
checkIO name action = (name, action)

runAll :: [Check] -> IO [(String, Either String ())]
runAll = mapM $ \(name, action) -> do
  outcome <- try (guarded name action) :: IO (Either SomeException ())
  pure (name, either (Left . displayException) Right outcome)

guarded :: String -> IO a -> IO a
guarded label action = do
  result <- timeout (120 * 1000000) action
  maybe (fail (label <> ": timed out after 120s")) pure result

checks :: ClickhouseConnectionSettings ClientHTTP -> Bool -> [Check]
checks conn allowWrites =
  readOnlyChecks conn <> [check | allowWrites, check <- writeChecks conn]

readOnlyChecks :: ClickhouseConnectionSettings ClientHTTP -> [Check]
readOnlyChecks conn =
  [ checkIO "large SELECT streams through a strict fold (no row list retained)" $
      checkLargeStreamedFold conn
  , checkIO "rows arrive before the response completes" $
      checkFirstRowsEarly conn
  , checkIO "early termination cancels the transfer and keeps the agent usable" $
      checkEarlyTermination conn
  , checkIO "server errors surface as ClickhouseServerException and keep the agent usable" $
      checkServerError conn
  , checkIO "truncated response body reports a transport error and keeps the agent usable" $
      checkTruncatedBody conn
  , checkIO "external tables (RowBinary multipart) round trip" $
      checkExternalTables conn
  , checkIO "typed query parameters bind scalars, escaped strings and arrays" $
      checkTypedQueryParams conn
  , checkIO "typed query parameters coexist with RowBinary external tables" $
      checkTypedParamsWithExternals conn
  ]

settingsChecks :: ClickhouseConnectionSettings ClientHTTP -> Maybe String -> [Check]
settingsChecks conn readonlyUser =
  [ checkIO "ordinary SELECT and zero-row result use no codec settings" $ do
      rows <- runQuery conn "SELECT toUInt64(42)"
      assertEqIO "ordinary row" (rowsVector [[ClickUInt64 42]]) rows
      empty <- runQuery conn "SELECT toUInt64(42) WHERE 0"
      assertEqIO "empty result" Vector.empty empty
  , checkIO "DateTime timezone external schema works without JSON settings" $ do
      let external = externalTable "timezone_input" [("v", "DateTime('UTC')")] [[ClickDateTime (read "2020-09-13 12:26:40 UTC")]]
      rows <- runQueryWithExternals conn [external] "SELECT toString(v) FROM timezone_input"
      assertEqIO "timezone value" (rowsVector [[ClickString "2020-09-13 12:26:40"]]) rows
  , checkIO "non-JSON external type forms do not require decoder support" $ do
      forM_
        [ ("Decimal32(2)", ClickDecimal32 123, "1.23")
        , ("Tuple(JSON UInt64, label String)", ClickTuple (Vector.fromList [ClickUInt64 7, ClickString "x"]), "(7,'x')")
        , ("SimpleAggregateFunction(sum, UInt64)", ClickUInt64 7, "7")
        , ("Enum8('JSON' = -1, 'other' = 1)", ClickInt8 (-1), "JSON")
        ]
        $ \(typeName, value, expected) -> do
          let external = externalTable "typed_input" [("v", typeName)] [[value]]
          rows <- runQueryWithExternals conn [external] "SELECT toString(v) FROM typed_input"
          assertEqIO (BSC.unpack typeName) (rowsVector [[ClickString expected]]) rows
  , checkIO "typed settings, URL strings, placeholders and overrides reach ClickHouse" $ do
      let configured = conn{settings = [("max_threads", ClickUInt64 3), ("log_comment", ClickString "a &+%?#='\\"), ("max_execution_time", ClickFloat64 1.25)]}
          request =
            (selectRequest "SELECT toUInt64(getSetting('max_threads')), getSetting('log_comment'), getSetting('max_execution_time'), {value:String}")
              { requestParams = [("max_threads", "2"), ("param_value", "x &+%?#='\\\\")]
              }
      rows <- runResourceT (runConduit (sourceRequest configured request .| sinkList))
      assertEqIO "effective values" [Vector.fromList [ClickUInt64 2, ClickString "a &+%?#='\\", ClickFloat64 1.25, ClickString "x &+%?#='\\"]] rows
  , checkIO "invalid typed settings fail before connecting" $ do
      let transport = connectionSettings conn
          unreachable = conn{connectionSettings = transport{transportOptions = (transportOptions transport){clickhouseUrl = "http://127.0.0.1", port = 1}}, settings = [("max_threads", ClickArray Vector.empty)]}
      outcome <- try (runQuery unreachable "SELECT 1")
      case outcome of
        Left (ClickhouseSettingsException message) | "max_threads" `isInfixOf` message -> pure ()
        _ -> fail "invalid setting did not fail before HTTP"
  , checkIO "JSON output requires an explicit effective output flag" $ do
      let sql = "SELECT '{\"n\":1}'::JSON"
          enabled = conn{settings = [("output_format_binary_write_json_as_string", ClickBool True)]}
      rows <- runQuery enabled sql
      case map Vector.toList (Vector.toList rows) of
        [[ClickJSON _]] -> pure ()
        other -> fail ("unexpected JSON result: " <> show other)
      empty <- runQuery enabled (sql <> " WHERE 0")
      assertEqIO "empty JSON" Vector.empty empty
      nested <- runQuery enabled "SELECT ([ '{\"n\":1}'::JSON ], map('key', '{\"n\":1}'::JSON))"
      case map Vector.toList (Vector.toList nested) of
        [[ClickTuple fields]] | Vector.length fields == 2 -> pure ()
        other -> fail ("unexpected nested JSON result: " <> show other)
      forM_ [conn, conn{settings = [("output_format_binary_write_json_as_string", ClickBool False)]}, conn{settings = [("input_format_binary_read_json_as_string", ClickBool True)]}] $ \disabled ->
        forM_ [sql, sql <> " WHERE 0", "SELECT ['{}'::JSON]"] $ \query -> expectJSONError (runQuery disabled query)
      let overridden = (selectRequest sql){requestParams = [("output_format_binary_write_json_as_string", "0")]}
      expectJSONError (runResourceT (runConduit (sourceRequest enabled overridden .| sinkList)))
  , checkIO "external JSON input and output flags are independent" $ do
      let output = conn{settings = [("output_format_binary_write_json_as_string", ClickBool True)]}
          input = conn{settings = [("input_format_binary_read_json_as_string", ClickBool True)]}
          both = conn{settings = settings input <> settings output}
      jsonRows <- runQuery output "SELECT '{\"n\":1}'::JSON"
      let external = externalTable "json_input" [("value", "JSON")] (map Vector.toList (Vector.toList jsonRows))
      rejected <- try (runQueryWithExternals output [external] "SELECT value FROM json_input")
      case rejected of
        Left (ClickhouseSettingsException message) | "input_format_binary_read_json_as_string=1" `isInfixOf` message -> pure ()
        _ -> fail "external JSON accepted without input setting"
      stringRows <- runQueryWithExternals input [external] "SELECT toString(value) FROM json_input"
      assertEqIO "input JSON value" (rowsVector [[ClickString "{\"n\":1}"]]) stringRows
      echoed <- runQueryWithExternals both [external] "SELECT value FROM json_input"
      assertEqIO "JSON roundtrip" jsonRows echoed
      rejectedInsert <- try (runInsert output "not_contacted" ["value"] (map Vector.toList (Vector.toList jsonRows)))
      case rejectedInsert of
        Left (ClickhouseSettingsException message) | "input_format_binary_read_json_as_string=1" `isInfixOf` message -> pure ()
        _ -> fail "JSON INSERT accepted without input setting"
  , checkIO "empty external schemas retain nested JSON protection before HTTP" $ do
      let transport = connectionSettings conn
          unreachable = conn{connectionSettings = transport{transportOptions = (transportOptions transport){clickhouseUrl = "http://127.0.0.1", port = 1}}}
      forM_ ["Array(Tuple(value JSON))", "Tuple(JSON UInt64, value Array(JSON))", "Map(String, JSON)", "FutureWrapper(Tuple(`value` JSON))", "Array(Tuple(value jSoN))"] $ \typeName -> do
        let external = externalTable "empty_input" [("v", typeName)] []
        rejected <- try (runQueryWithExternals unreachable [external] "SELECT toString(v) FROM empty_input")
        case rejected of
          Left (ClickhouseSettingsException message) | "input_format_binary_read_json_as_string=1" `isInfixOf` message -> pure ()
          _ -> fail ("nested JSON schema bypassed the input guard: " <> BSC.unpack typeName)
  ]
    <> [ checkIO "readonly=1 user performs SELECT without modifying server settings" $ do
           let readonly = conn{username = Text.pack user, settings = []}
           rows <- runQuery readonly "SELECT toUInt64(getSetting('readonly')), toUInt64(42)"
           assertEqIO "readonly query" (rowsVector [[ClickUInt64 1, ClickUInt64 42]]) rows
           empty <- runQuery readonly "SELECT toUInt64(42) WHERE 0"
           assertEqIO "readonly empty query" Vector.empty empty
       | Just user <- [readonlyUser]
       ]
  where
    expectJSONError action = do
      outcome <- try action
      case outcome of
        Left (ClickhouseDecodeException message) | "output_format_binary_write_json_as_string=1" `isInfixOf` message -> pure ()
        _ -> fail "missing actionable JSON output-setting error"

writeChecks :: ClickhouseConnectionSettings ClientHTTP -> [Check]
writeChecks conn =
  [ checkIO "insert + select round trip (unique owned table)" $
      checkInsertRoundTrip conn
  , checkIO "large insert payload round trip (unique owned table)" $
      checkLargeInsert conn
  , checkIO "insert with typed parameters falls back to URL parameters (unique owned table)" $
      checkInsertWithParams conn
  ]

foldNumberRows :: (Monad m) => ConduitT (Vector.Vector ClickhouseType) o m (Int, Integer)
foldNumberRows = go 0 0
  where
    go !rowCount !total = await >>= \case
      Nothing -> pure (rowCount, total)
      Just row -> case Vector.toList row of
        [ClickUInt64 value] -> go (rowCount + 1) (total + toInteger value)
        other -> error ("unexpected row shape: " <> show other)

checkLargeStreamedFold :: ClickhouseConnectionSettings ClientHTTP -> IO ()
checkLargeStreamedFold conn = do
  let rowCount = 1000000 :: Int
      expectedSum = toInteger rowCount * toInteger (rowCount - 1) `div` 2
  baseline <- rtsMaxLiveBytes
  startedAt <- nowSeconds
  (foldedRows, foldedSum) <-
    runResourceT $
      runConduit $
        sourceQuery conn (BSC.pack ("SELECT number FROM numbers(" <> show rowCount <> ")"))
          .| foldNumberRows
  finishedAt <- nowSeconds
  assertEqIO "folded row count" rowCount foldedRows
  assertEqIO "folded sum" expectedSum foldedSum
  peak <- rtsMaxLiveBytes
  putStrLn ("  [evidence] folded 1e6 rows in " <> showMs (finishedAt - startedAt))
  case (baseline, peak) of
    (Just before, Just after) -> do
      let growth = after - before
      putStrLn
        ( "  [evidence] max live bytes grew by "
            <> show growth
            <> " B during the fold"
        )
      unless (growth < 64 * 1024 * 1024) $
        fail
          ( "live heap grew by " <> show growth
              <> " bytes during the fold; the result set looks materialised"
          )
    _ -> putStrLn "  [evidence] RTS statistics disabled; skipping the memory assertion"

firstAndLastRowTimes :: (MonadIO m) => ConduitT a o m (Double, Double, Int)
firstAndLastRowTimes = go Nothing 0
  where
    go !firstRowAt !rowCount = await >>= \case
      Nothing -> do
        finishedAt <- liftIO nowSeconds
        pure (maybe finishedAt id firstRowAt, finishedAt, rowCount)
      Just _ -> do
        receivedAt <- liftIO nowSeconds
        go (Just (maybe receivedAt id firstRowAt)) (rowCount + 1)

checkFirstRowsEarly :: ClickhouseConnectionSettings ClientHTTP -> IO ()
checkFirstRowsEarly conn = do
  let query = "SELECT number, sleepEachRow(0.05) FROM numbers(40) SETTINGS max_block_size=1"
  startedAt <- nowSeconds
  (firstRowAt, finishedAt, rowCount) <-
    runResourceT (runConduit (sourceQuery conn query .| firstAndLastRowTimes))
  let total = finishedAt - startedAt
      first = firstRowAt - startedAt
  putStrLn
    ( "  [evidence] first row after "
        <> showMs first
        <> ", stream complete after "
        <> showMs total
    )
  assertEqIO "row count" 40 rowCount
  unless (total > 1.0) $
    fail ("query finished in " <> show total <> "s; too fast to prove incremental delivery")
  unless (first < total / 2) $
    fail
      ( "first row arrived after " <> show first <> "s of a " <> show total
          <> "s response"
      )

checkEarlyTermination :: ClickhouseConnectionSettings ClientHTTP -> IO ()
checkEarlyTermination conn = do
  let slowQuery = "SELECT number, sleepEachRow(0.05) FROM numbers(20000) SETTINGS max_block_size=1"
  startedAt <- nowSeconds
  taken <-
    runResourceT $
      runConduit $
        sourceQuery conn slowQuery .| (ConduitC.take 3 .| sinkList)
  abortedAt <- nowSeconds
  assertEqIO "rows taken before aborting" 3 (length taken)
  putStrLn ("  [evidence] aborted a ~1000s stream after " <> showMs (abortedAt - startedAt))
  unless (abortedAt - startedAt < 10) $
    fail ("abort took " <> show (abortedAt - startedAt) <> "s; the transfer was not cancelled")
  rows <- runQuery conn "SELECT count() FROM numbers(100)"
  assertEqIO "same-agent follow-up after abort" (rowsVector [[ClickUInt64 100]]) rows

uniqueTableName :: String -> IO Text
uniqueTableName prefix = do
  unique <- hashUnique <$> newUnique
  nanos <- getMonotonicTimeNSec
  pure (Text.pack (prefix <> "_" <> show unique <> "_" <> show nanos))

withOwnedTable ::
  ClickhouseConnectionSettings ClientHTTP ->
  Text ->
  ByteString ->
  IO () ->
  IO ()
withOwnedTable conn tableName createStatement action =
  bracket
    (runCommand conn createStatement)
    (\() -> runCommand conn ("DROP TABLE " <> Text.encodeUtf8 tableName))
    (const action)

checkInsertRoundTrip :: ClickhouseConnectionSettings ClientHTTP -> IO ()
checkInsertRoundTrip conn = do
  tableName <- uniqueTableName "it_roundtrip"
  let table = Text.encodeUtf8 tableName
      createStatement = "CREATE TABLE " <> table <> " (n UInt64, s String) ENGINE = Memory"
  withOwnedTable conn tableName createStatement $ do
    let rows =
          [ [ ClickUInt64 (fromIntegral rowIndex)
            , ClickString (BSC.pack ("row-" <> show rowIndex))
            ]
          | rowIndex <- [1 .. 1000 :: Int]
          ]
    runInsert conn tableName ["n", "s"] rows
    got <- runQuery conn ("SELECT count(), sum(n) FROM " <> table)
    assertEqIO
      "count/sum after insert"
      (rowsVector [[ClickUInt64 1000, ClickUInt64 500500]])
      got

checkLargeInsert :: ClickhouseConnectionSettings ClientHTTP -> IO ()
checkLargeInsert conn = do
  tableName <- uniqueTableName "it_bulk"
  let table = Text.encodeUtf8 tableName
      rowCount = 400000 :: Int
      expectedSum = toInteger rowCount * toInteger (rowCount + 1) `div` 2
      createStatement = "CREATE TABLE " <> table <> " (n UInt64, s String) ENGINE = Memory"
  withOwnedTable conn tableName createStatement $ do
    let rows =
          [ [ ClickUInt64 (fromIntegral rowIndex)
            , ClickString (BSC.pack ("v" <> show rowIndex))
            ]
          | rowIndex <- [1 .. rowCount]
          ]
        payloadBytes = sum (map rowSize rows)
        rowSize [ClickUInt64 _, ClickString stringValue] = 8 + 1 + BS.length stringValue
        rowSize _ = 0
    startedAt <- nowSeconds
    runInsert conn tableName ["n", "s"] rows
    finishedAt <- nowSeconds
    got <- runQuery conn ("SELECT count(), sum(n) FROM " <> table)
    assertEqIO
      "count/sum after bulk insert"
      (rowsVector [[ClickUInt64 (fromIntegral rowCount), ClickUInt64 (fromInteger expectedSum)]])
      got
    putStrLn
      ( "  [evidence] inserted "
          <> show rowCount
          <> " rows (~"
          <> show (payloadBytes `div` (1024 * 1024))
          <> " MiB RowBinary) in "
          <> showMs (finishedAt - startedAt)
      )

checkServerError :: ClickhouseConnectionSettings ClientHTTP -> IO ()
checkServerError conn = do
  outcome <-
    try (runQuery conn "SELECT * FROM __ch_driver_missing_table__")
      :: IO (Either SomeException (Vector.Vector (Vector.Vector ClickhouseType)))
  case outcome of
    Left exception
      | Just serverError <- (fromException exception :: Maybe ClickhouseServerException) -> do
          unless (serverStatus serverError >= 400) $
            fail ("unexpected status " <> show (serverStatus serverError))
          unless ("Unknown" `BS.isInfixOf` serverMessage serverError) $
            fail ("unexpected server error body: " <> show (serverMessage serverError))
          putStrLn
            ( "  [evidence] ClickhouseServerException "
                <> show (serverStatus serverError)
                <> ": "
                <> takeByteString 80 (serverMessage serverError)
            )
      | otherwise ->
          fail ("expected ClickhouseServerException, got " <> displayException exception)
    Right _ -> fail "expected an exception for a missing table"
  rows <- runQuery conn "SELECT 42"
  assertEqIO
    "same-agent follow-up after server error"
    (rowsVector [[ClickUInt8 42]])
    rows

checkTruncatedBody :: ClickhouseConnectionSettings ClientHTTP -> IO ()
checkTruncatedBody conn = do
  (port, stopServer) <- startTruncatingServer
  let transport = connectionSettings conn
      fakeConnection =
        conn
          { connectionSettings =
              transport
                { transportOptions =
                    (transportOptions transport)
                      { clickhouseUrl = "http://127.0.0.1"
                      , port = port
                      }
                }
          }
  outcome <-
    try
      ( runResourceT
          (runConduit (sendSource fakeConnection (commandRequest "SELECT 1") .| sinkNull))
      )
      :: IO (Either SomeException ())
  stopServer
  case outcome of
    Left exception
      | Just transportError <- (fromException exception :: Maybe ClickhouseTransportException) ->
          putStrLn
            ( "  [evidence] truncated body raised "
                <> take 120 (transportMessage transportError)
            )
      | otherwise ->
          fail ("expected ClickhouseTransportException, got " <> displayException exception)
    Right () -> fail "the truncated response body was not reported"
  rows <- runQuery conn "SELECT count() FROM numbers(5)"
  assertEqIO
    "same-agent follow-up after truncation"
    (rowsVector [[ClickUInt64 5]])
    rows

startTruncatingServer :: IO (Int, IO ())
startTruncatingServer = do
  listener <- socket AF_INET Stream defaultProtocol
  setSocketOption listener ReuseAddr 1
  bind listener (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
  listen listener 1
  port <- getSocketName listener >>= \case
    SockAddrInet rawPort _ -> pure (fromIntegral rawPort)
    _ -> fail "unexpected listener address family"
  worker <- forkIO (serveTruncated listener `finally` quietClose listener)
  pure (port, killThread worker >> quietClose listener)

serveTruncated :: Socket -> IO ()
serveTruncated listener =
  bracket (fst <$> accept listener) quietClose $ \client -> do
    drainRequest client
    SocketBS.sendAll
      client
      "HTTP/1.1 200 OK\r\nContent-Length: 1000\r\nConnection: close\r\n\r\n"
    SocketBS.sendAll client (BS.replicate 10 65)

drainRequest :: Socket -> IO ()
drainRequest client = go BS.empty
  where
    go received = do
      chunk <- SocketBS.recv client 4096
      if BS.null chunk
        then pure ()
        else do
          let receivedNow = received <> chunk
          case BSC.breakSubstring "\r\n\r\n" receivedNow of
            (headers, rest)
              | not (BS.null rest) ->
                  let body = BS.drop 4 rest
                   in if BS.length body >= contentLength headers
                        then pure ()
                        else go receivedNow
            _ -> go receivedNow
    contentLength headers =
      case
          [ value
          | line <- BSC.lines headers
          , let (key, value) = BSC.break (== ':') line
          , BSC.map toLower key == "content-length"
          ]
        of
        (value : _) -> case BSC.readInt (BSC.dropWhile (== ' ') value) of
          Just (len, _) -> len
          Nothing -> 0
        [] -> 0

checkExternalTables :: ClickhouseConnectionSettings ClientHTTP -> IO ()
checkExternalTables conn = do
  let ids =
        externalTable
          "ids"
          [("value", "UInt64")]
          [[ClickUInt64 1], [ClickUInt64 2], [ClickUInt64 3]]
  streamed <-
    runResourceT $
      runConduit $
        sourceQueryWithExternals conn [ids] "SELECT value FROM ids ORDER BY value"
          .| foldNumberRows
  assertEqIO "streamed external table" (3, 6) streamed
  let names =
        externalTable
          "names"
          [("name", "String")]
          [[ClickString "a"], [ClickString "bb"], [ClickString "ccc"]]
  got <- runQueryWithExternals conn [names] "SELECT count(), sum(length(name)) FROM names"
  assertEqIO
    "string external table"
    (rowsVector [[ClickUInt64 3, ClickUInt64 6]])
    got
  let single = scalarExternal "single" "UInt64" (ClickUInt64 7)
  gotScalar <-
    runQueryWithExternals
      conn
      [single]
      "SELECT count() FROM numbers(100) WHERE number % 10 = (SELECT value FROM single)"
  assertEqIO "scalar external table" (rowsVector [[ClickUInt64 10]]) gotScalar

-- | Typed @{name:Type}@ values must arrive through escaped text, so every
-- case below is chosen such that a wrong escape changes the returned bytes.
checkTypedQueryParams :: ClickhouseConnectionSettings ClientHTTP -> IO ()
checkTypedQueryParams conn = do
  scalar <- runQueryWithParams conn [("p", ClickUInt64 41)] "SELECT {p:UInt64} + 1"
  assertEqIO "scalar parameter" (rowsVector [[ClickUInt64 42]]) scalar
  putStrLn ("  [evidence] SELECT {p:UInt64} + 1 with param_p=41 -> " <> show (firstRow scalar))
  let tricky =
        BS.pack
          [ 0x61, 0x09, 0x62, 0x0A, 0x63, 0x0D, 0x64, 0x5C, 0x65, 0x00
          , 0x66, 0x01, 0x67, 0x7F, 0x68
          , 0xC3, 0xA9
          , 0x27, 0xFF
          ]
  escaped <-
    runQueryWithParams
      conn
      [("s", ClickString tricky)]
      "SELECT hex({s:String}), length({s:String})"
  assertEqIO
    "escaped string round trip"
    (rowsVector [[ClickString (BSC.map toUpper (hexOf tricky)), ClickUInt64 (fromIntegral (BS.length tricky))]])
    escaped
  putStrLn
    ( "  [evidence] SELECT hex({s:String}), length({s:String}) for "
        <> show (BS.length tricky)
        <> " bytes (tab/LF/CR/backslash/NUL/0x01/0x7F/UTF-8/quote/0xFF) -> "
        <> show (firstRow escaped)
    )
  arrays <-
    runQueryWithParams
      conn
      [("ids", ClickArray (Vector.fromList [ClickUInt64 1, ClickUInt64 2, ClickUInt64 3]))]
      "SELECT length({ids:Array(UInt64)}), arraySum({ids:Array(UInt64)})"
  assertEqIO "array parameter" (rowsVector [[ClickUInt64 3, ClickUInt64 6]]) arrays
  putStrLn
    ( "  [evidence] SELECT length, arraySum over {ids:Array(UInt64)} = [1,2,3] -> "
        <> show (firstRow arrays)
    )
  let names =
        Vector.fromList
          [ ClickString "it's"
          , ClickString "a\\b"
          , ClickString (BS.pack [0x61, 0x0A, 0x62])
          ]
  nested <- runQueryWithParams conn [("names", ClickArray names)] "SELECT {names:Array(String)}"
  assertEqIO "quoted strings inside an array" (rowsVector [[ClickArray names]]) nested
  putStrLn ("  [evidence] nested quoting of {names:Array(String)} -> " <> show (firstRow nested))
  typed <-
    runQueryWithParams
      conn
      [ ("b", ClickBool True)
      , ("u", ClickUuid (fromWords64 0x550E8400E29B41D4 0xA716446655440000))
      , ("t", ClickDateTime64 3 (posixSecondsToUTCTime (fromRational (1638543825123 % 1000))))
      , ("n", ClickNullable Nothing)
      , ("s", ClickNullable (Just (ClickString "x")))
      ]
      "SELECT toString({b:Bool}), toString({u:UUID}), toUnixTimestamp64Milli({t:DateTime64(3)}), \
      \isNull({n:Nullable(UInt8)}), toString({s:Nullable(String)})"
  assertEqIO
    "Bool/UUID/DateTime64/Nullable parameters"
    ( rowsVector
        [ [ ClickString "true"
          , ClickString "550e8400-e29b-41d4-a716-446655440000"
          , ClickInt64 1638543825123
          , ClickUInt8 1
          , ClickNullable (Just (ClickString "x"))
          ]
        ]
    )
    typed
  putStrLn ("  [evidence] Bool/UUID/DateTime64/Nullable bindings -> " <> show (firstRow typed))

-- | The multipart body carries the @query@ field, the typed
-- @param_<name>@ fields and the RowBinary external parts at the same time.
checkTypedParamsWithExternals :: ClickhouseConnectionSettings ClientHTTP -> IO ()
checkTypedParamsWithExternals conn = do
  let ids =
        externalTable
          "ids"
          [("value", "UInt64")]
          [[ClickUInt64 1], [ClickUInt64 2], [ClickUInt64 3]]
      request =
        (externalSelectRequest [ids] "SELECT {base:UInt64} + sum(value) AS total, count() AS cnt FROM ids")
          { requestQueryParams = [("base", ClickUInt64 40)]
          }
  rows <- runResourceT (runConduit (sourceRequest conn request .| sinkList))
  assertEqIO
    "typed parameter plus external table"
    (rowsVector [[ClickUInt64 46, ClickUInt64 3]])
    (Vector.fromList rows)
  putStrLn
    ( "  [evidence] multipart query + param_base=40 + RowBinary external part -> "
        <> show (firstRow (Vector.fromList rows))
    )

-- | An INSERT payload cannot live in a multipart body, so typed parameters
-- fall back to URL parameters there.  The inserted values depend on both the
-- external payload (RowBinary) and the URL parameter.
checkInsertWithParams :: ClickhouseConnectionSettings ClientHTTP -> IO ()
checkInsertWithParams conn = do
  tableName <- uniqueTableName "it_params"
  let table = Text.encodeUtf8 tableName
      createStatement = "CREATE TABLE " <> table <> " (n UInt64, tag String) ENGINE = Memory"
  withOwnedTable conn tableName createStatement $ do
    let payload = encodeRowsWithSettings [] (map (Vector.singleton . ClickString) ["x", "yy"])
        statement =
          "INSERT INTO " <> table
            <> " (n, tag) SELECT {add:UInt64} + toUInt64(length(tag) - 1) AS n, tag"
            <> " FROM input('tag String') FORMAT RowBinary"
        request = (insertRequest statement payload) {requestQueryParams = [("add", ClickUInt64 40)]}
    runResourceT (runConduit (sendSource conn request .| sinkNull))
    rows <- runQuery conn ("SELECT n, tag FROM " <> table <> " ORDER BY tag")
    assertEqIO
      "insert with typed parameters"
      (rowsVector [[ClickUInt64 40, ClickString "x"], [ClickUInt64 41, ClickString "yy"]])
      rows
    putStrLn
      ( "  [evidence] INSERT ... input('tag String') FORMAT RowBinary with param_add=40 in the URL -> "
          <> show rows
      )

firstRow :: Vector.Vector (Vector.Vector ClickhouseType) -> [ClickhouseType]
firstRow rows = maybe [] Vector.toList (rows Vector.!? 0)

hexOf :: ByteString -> ByteString
hexOf = BSC.concat . map hexByte . BS.unpack
 where
  hexByte byte = BSC.pack (padLeft 2 (showHex byte ""))
  padLeft width text
    | length text >= width = text
    | otherwise = replicate (width - length text) '0' <> text

rowsVector :: [[ClickhouseType]] -> Vector.Vector (Vector.Vector ClickhouseType)
rowsVector = Vector.fromList . map Vector.fromList

rtsMaxLiveBytes :: IO (Maybe Word64)
rtsMaxLiveBytes = do
  enabled <- getRTSStatsEnabled
  if enabled
    then Just . max_live_bytes <$> getRTSStats
    else pure Nothing

nowSeconds :: IO Double
nowSeconds = (/ 1e9) . fromIntegral <$> getMonotonicTimeNSec

showMs :: Double -> String
showMs seconds =
  show (fromIntegral (round (seconds * 1000000) :: Integer) / 1000 :: Double) <> "ms"

assertEqIO :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqIO label expected actual =
  unless (expected == actual) $
    fail (label <> ": expected " <> show expected <> ", got " <> show actual)

takeByteString :: Int -> ByteString -> String
takeByteString count = BSC.unpack . BS.take count

quietClose :: Socket -> IO ()
quietClose sock = void (try (close sock) :: IO (Either SomeException ()))
