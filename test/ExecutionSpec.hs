{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module ExecutionSpec (executionChecks) where

import Control.Concurrent (forkFinally, killThread)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (AsyncException (ThreadKilled), Exception, SomeException, displayException, finally, fromException, throwIO, try)
import Control.Monad (unless, void, when)
import Control.Monad.Catch (Handler (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Resource (register, runResourceT)
import Control.Retry (constantDelay, limitRetries, recovering)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as BSL
import Data.Conduit (runConduit, yield, (.|))
import Data.Conduit.Combinators qualified as Conduit
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Database.ClickHouse
import Database.Clickhouse.Conversion.Binary.Encode (encodeLEB128, encodeRows, encodeValue)
import HTTPMetricsSpec (receiveBody, withLocalServers)
import Network.Socket (Socket)
import Network.Socket.ByteString qualified as SocketBS
import System.Timeout (timeout)

executionChecks :: [(String, IO (Either String ()))]
executionChecks =
  [ check "custom execution supplies RowBinary without an HTTP transport" customExecution
  , check "legacy clients retain default whole-request execution" legacyCollectors
  , check "all collectors invoke the execution wrapper with their request" wrappedCollectors
  , check "default execution forwards updated shared connection fields" connectionUpdates
  , check "record update replaces the transport source" sourceOverride
  , check "execution wrapper encloses successful ResourceT cleanup" (lifecycle False)
  , check "execution wrapper encloses failing ResourceT cleanup" (lifecycle True)
  , check "retry discards partial results and cleans up before another attempt" retryPartial
  , check "retry exhaustion propagates the final exception" retryExhaustion
  , check "retry does not catch nonmatching decode failures" decodeFailure
  , check "default execution never retries queries or writes" noImplicitRetry
  , check "asynchronous cancellation propagates and releases the attempt" asynchronousCancellation
  , check "streaming helpers bypass the whole-request wrapper" streamingBypass
  , check "early streaming close releases the custom transport" earlyStreamCleanup
  , check "streaming failure never replays already delivered rows" streamingFailure
  , check "nested default executions preserve middleware" nestedExecutions
  , check "stock HTTP retry preserves hooks and per-attempt final metrics" httpRetry
  ]
 where
  check name action =
    ( name
    , do
        result <- try @SomeException $ timeout 5_000_000 action
        pure $ case result of
          Left exception -> Left (displayException exception)
          Right Nothing -> Left "execution check timed out"
          Right (Just ()) -> Right ()
    )

data FixtureClient

newtype FixtureTransport = FixtureTransport (IORef [(ClickhouseConnectionSettings FixtureClient, CHRequest)])

instance ClickhouseClient FixtureClient where
  type ClickhouseClientSettings FixtureClient = FixtureTransport

  sendSource connection request = do
    let FixtureTransport requests = connectionSettings connection
    liftIO $ append requests (connection, request)
    yield (response [1, 2])

data TransientFailure = TransientFailure Int
  deriving (Eq, Show)

instance Exception TransientFailure

fixtureConnection :: IO (ClickhouseConnectionSettings FixtureClient, IORef [(ClickhouseConnectionSettings FixtureClient, CHRequest)])
fixtureConnection = do
  requests <- newIORef []
  pure (defaultConnection (FixtureTransport requests), requests)

customExecution :: IO ()
customExecution = do
  requests <- newIORef []
  let execution = executionFromSource \current request -> do
        liftIO $ assertEqual "custom connection database" "analytics" (database current)
        liftIO $ append requests request
        yield (response [1, 2])
      connection = (defaultConnection execution){database = "analytics"}
  runCollectors connection >>= \expected -> readIORef requests >>= assertRequests expected

legacyCollectors :: IO ()
legacyCollectors = do
  (connection, requests) <- fixtureConnection
  expected <- runCollectors connection
  readIORef requests >>= assertRequests expected . map snd

wrappedCollectors :: IO ()
wrappedCollectors = do
  (base, requests) <- fixtureConnection
  wrapped <- newIORef []
  let normal = defaultExecution base
      execution =
        normal
          { executeRequest = \connection request action -> do
              append wrapped request
              executeRequest normal connection request action
          }
  expected <- runCollectors (withExecution execution base)
  readIORef wrapped >>= assertRequests expected
  readIORef requests >>= assertRequests expected . map snd

runCollectors :: ClickhouseClient client => ClickhouseConnectionSettings client -> IO [CHRequest]
runCollectors connection = do
  let params = [("value", ClickUInt64 2)]
      externals = [scalarExternal "values" "UInt64" (ClickUInt64 2)]
      raw = (selectRequest "SELECT value"){requestParams = [("max_threads", "1")]}
      queries =
        [ runQuery connection "SELECT value"
        , runQueryWithParams connection params "SELECT {value:UInt64}"
        , runQueryWithExternals connection externals "SELECT value FROM values"
        , runRequest connection raw
        ]
  mapM_ (\query -> query >>= assertEqual "collected rows" (rows [1, 2])) queries
  runInsert connection "values" ["value"] [[ClickUInt64 1]]
  runCommand connection "CREATE TABLE values (value UInt64) ENGINE = Memory"
  pure
    [ selectRequest "SELECT value"
    , selectRequestWithParams params "SELECT {value:UInt64}"
    , externalSelectRequest externals "SELECT value FROM values"
    , raw
    , insertRequest "INSERT INTO values (\"value\") FORMAT RowBinary" (encodeRows [Vector.singleton (ClickUInt64 1)])
    , commandRequest "CREATE TABLE values (value UInt64) ENGINE = Memory"
    ]

connectionUpdates :: IO ()
connectionUpdates = do
  (base, requests) <- fixtureConnection
  wrapped <- newIORef []
  let normal = defaultExecution base
      execution =
        normal
          { executeRequest = \current request action -> do
              append wrapped current
              executeRequest normal current request action
          }
      connection =
        (withExecution execution base)
          { username = "updated-user"
          , password = "updated-password"
          , database = "updated-database"
          , settings = [("max_threads", ClickUInt8 1)]
          , extraHeaders = [("x-request-id", "updated-request")]
          }
  void $ runQuery connection "SELECT value"
  readIORef requests >>= mapM_ (assertConnectionFields connection . fst)
  readIORef wrapped >>= mapM_ (assertConnectionFields connection)

sourceOverride :: IO ()
sourceOverride = do
  (base, requests) <- fixtureConnection
  let normal = defaultExecution base
      execution = normal{executeSource = \_ _ -> yield (response [9])}
  runQuery (withExecution execution base) "SELECT value" >>= assertEqual "replaced source" (rows [9])
  readIORef requests >>= assertEqual "stock source calls" 0 . length

lifecycle :: Bool -> IO ()
lifecycle fails = do
  events <- newIORef ([] :: [String])
  let normal = executionFromSource \_ _ -> do
        liftIO $ append events "acquire"
        void $ lift $ register (append events "release")
        yield (response [1])
        when fails $ liftIO $ throwIO (TransientFailure 1)
      execution =
        normal
          { executeRequest = \connection request action -> do
              append events "before"
              executeRequest normal connection request action `finally` append events "after"
          }
  result <- try @TransientFailure $ runQuery (defaultConnection execution) "SELECT value"
  if fails
    then assertEqual "failing request" (Left (TransientFailure 1)) result
    else assertEqual "successful request" (Right (rows [1])) result
  readIORef events >>= assertEqual "execution/resource order" ["before", "acquire", "release", "after"]

retryPartial :: IO ()
retryPartial = do
  attempts <- newIORef 0
  releases <- newIORef 0
  let normal = executionFromSource \_ _ -> do
        attempt <- liftIO $ increment attempts
        liftIO $ readIORef releases >>= assertEqual "previous attempt released" (attempt - 1)
        void $ lift $ register (void $ increment releases)
        yield (response [1])
        when (attempt == 1) $ liftIO $ throwIO (TransientFailure attempt)
        yield (encodeRows [Vector.singleton (ClickUInt64 2)])
      connection = defaultConnection (retryExecution 1 normal)
  runQuery connection "SELECT value" >>= assertEqual "no partial row duplication" (rows [1, 2])
  readIORef attempts >>= assertEqual "attempt count" 2
  readIORef releases >>= assertEqual "released attempts" 2

retryExhaustion :: IO ()
retryExhaustion = do
  attempts <- newIORef 0
  releases <- newIORef 0
  let normal = executionFromSource \_ _ -> do
        void $ lift $ register (void $ increment releases)
        attempt <- liftIO $ increment attempts
        liftIO $ throwIO (TransientFailure attempt)
  result <- try @TransientFailure $ runQuery (defaultConnection $ retryExecution 2 normal) "SELECT value"
  assertEqual "last retry error" (Left $ TransientFailure 3) result
  readIORef attempts >>= assertEqual "bounded attempts" 3
  readIORef releases >>= assertEqual "exhausted attempt cleanup" 3

decodeFailure :: IO ()
decodeFailure = do
  attempts <- newIORef 0
  let normal = executionFromSource \_ _ -> do
        liftIO $ void $ increment attempts
        yield (response [1] <> "\0")
  result <- try @ClickhouseDecodeException $ runQuery (defaultConnection $ retryExecution 2 normal) "SELECT value"
  case result of
    Left _ -> pure ()
    Right _ -> fail "expected a truncated RowBinary decoding failure"
  readIORef attempts >>= assertEqual "nonmatching error attempts" 1

noImplicitRetry :: IO ()
noImplicitRetry = do
  attempts <- newIORef 0
  let execution = executionFromSource \_ _ -> do
        attempt <- liftIO $ increment attempts
        liftIO $ throwIO (TransientFailure attempt)
      connection = defaultConnection execution
  mapM_
    (\action -> try @TransientFailure action >>= either (const $ pure ()) (const $ fail "expected failure"))
    [ void $ runQuery connection "SELECT value"
    , runInsert connection "values" ["value"] [[ClickUInt64 1]]
    , runCommand connection "CREATE TABLE values (value UInt64) ENGINE = Memory"
    ]
  readIORef attempts >>= assertEqual "one attempt per query/write" 3

asynchronousCancellation :: IO ()
asynchronousCancellation = do
  arrived <- newEmptyMVar
  blocked <- newEmptyMVar
  completed <- newEmptyMVar
  attempts <- newIORef 0
  releases <- newIORef 0
  let normal = executionFromSource \_ _ -> do
        liftIO $ void $ increment attempts
        void $ lift $ register (void $ increment releases)
        liftIO $ putMVar arrived () >> takeMVar blocked
      connection = defaultConnection (retryExecution 2 normal)
  worker <- forkFinally (runQuery connection "SELECT value") (putMVar completed)
  finally
    ( do
        takeMVar arrived
        killThread worker
        takeMVar completed >>= \case
          Left exception | Just ThreadKilled <- fromException exception -> pure ()
          other -> fail ("cancellation was swallowed: " <> show other)
        readIORef attempts >>= assertEqual "cancellation attempts" 1
        readIORef releases >>= assertEqual "cancelled attempt cleanup" 1
    )
    (killThread worker)

streamingBypass :: IO ()
streamingBypass = do
  wrapped <- newIORef 0
  let normal = executionFromSource \_ _ -> yield (response [1, 2])
      execution =
        normal
          { executeRequest = \current request action -> do
              void $ increment wrapped
              executeRequest normal current request action
          }
      connection = defaultConnection execution
      streams =
        [ sourceQuery connection "SELECT value"
        , sourceQueryWithParams connection [("value", ClickUInt64 2)] "SELECT {value:UInt64}"
        , sourceQueryWithExternals connection [scalarExternal "values" "UInt64" (ClickUInt64 2)] "SELECT value FROM values"
        , sourceRequest connection (selectRequest "SELECT value")
        ]
  mapM_ (\source -> runResourceT (runConduit $ source .| Conduit.sinkList) >>= assertEqual "streamed rows" (Vector.toList $ rows [1, 2])) streams
  readIORef wrapped >>= assertEqual "whole-request wrapper calls" 0

earlyStreamCleanup :: IO ()
earlyStreamCleanup = do
  events <- newIORef ([] :: [String])
  let execution = executionFromSource \_ _ -> do
        void $ lift $ register (append events "release")
        yield (response [1])
        liftIO $ append events "continued"
        yield (encodeRows [Vector.singleton (ClickUInt64 2)])
  result <- runResourceT $ runConduit $ sourceQuery (defaultConnection execution) "SELECT value" .| Conduit.take 1 .| Conduit.sinkList
  assertEqual "stream prefix" (Vector.toList $ rows [1]) result
  readIORef events >>= assertEqual "early close cleanup without continuation" ["release"]

streamingFailure :: IO ()
streamingFailure = do
  delivered <- newIORef []
  attempts <- newIORef 0
  releases <- newIORef 0
  let normal = executionFromSource \_ _ -> do
        void $ lift $ register (void $ increment releases)
        attempt <- liftIO $ increment attempts
        yield (response [1])
        liftIO $ throwIO (TransientFailure attempt)
  result <-
    try @TransientFailure $
      runResourceT $
        runConduit $
          sourceQuery (defaultConnection $ retryExecution 2 normal) "SELECT value"
            .| Conduit.mapM_ (liftIO . append delivered)
  assertEqual "stream error" (Left $ TransientFailure 1) result
  readIORef delivered >>= assertEqual "rows delivered once" (Vector.toList $ rows [1])
  readIORef attempts >>= assertEqual "no streaming retry" 1
  readIORef releases >>= assertEqual "failed stream cleanup" 1

nestedExecutions :: IO ()
nestedExecutions = do
  events <- newIORef ([] :: [String])
  let source = executionFromSource \_ _ -> do
        liftIO $ append events "source"
        yield (response [1])
      middleware label normal =
        normal
          { executeRequest = \connection request action -> do
              append events (label <> " before")
              executeRequest normal connection request action `finally` append events (label <> " after")
          }
      inner = defaultConnection (middleware "inner" source)
      outer = withExecution (middleware "outer" $ defaultExecution inner) inner
  runQuery outer "SELECT value" >>= assertEqual "nested result" (rows [1])
  readIORef events >>= assertEqual "nested middleware order" ["outer before", "inner before", "source", "inner after", "outer after"]

httpRetry :: IO ()
httpRetry = withLocalServers 2_000 [reply 8 [1], reply 0 [1, 2]] \base -> do
  events <- newIORef []
  requests <- newIORef 0
  responses <- newIORef 0
  let config =
        defaultHTTPConfig
          { httpOnEvent = append events
          , httpModifyRequest = \_ native -> increment requests >> pure native
          , httpOnResponse = \_ _ -> void $ increment responses
          }
      configured = base{connectionSettings = withHTTPConfig config (connectionSettings base)}
      normal = defaultExecution configured
      execution =
        normal
          { executeRequest = \connection request action ->
              recovering
                (constantDelay 0 <> limitRetries 1)
                [const $ Handler \(_ :: ClickhouseTransportException) -> pure True]
                (\_ -> executeRequest normal connection request action)
          }
  runQuery (withExecution execution configured) "SELECT value" >>= assertEqual "HTTP retried rows" (rows [1, 2])
  readIORef requests >>= assertEqual "native request hooks" 2
  readIORef responses >>= assertEqual "native response hooks" 2
  recorded <- readIORef events
  assertEqual "terminal outcomes" [HTTPTransportFailed, HTTPTransferSucceeded] [outcome | HTTPRequestFinished _ outcome _ <- recorded]
  case recorded of
    [HTTPRequestStarted first _, HTTPResponseReceived firstResponse _, HTTPRequestFinished firstFinished _ _, HTTPRequestStarted second _, HTTPResponseReceived secondResponse _, HTTPRequestFinished secondFinished _ _] ->
      unless (first /= second && first == firstResponse && first == firstFinished && second == secondResponse && second == secondFinished) $ fail "retry lost correlated per-attempt events"
    _ -> fail ("unexpected HTTP retry lifecycle: " <> show recorded)
 where
  reply :: Int -> [Integer] -> Socket -> IO ()
  reply extra values socket = do
    void $ receiveBody socket BS.empty
    let payload = response values
    SocketBS.sendAll socket ("HTTP/1.1 200 OK\r\nContent-Length: " <> BSC.pack (show $ BS.length payload + extra) <> "\r\nConnection: close\r\n\r\n" <> payload)

retryExecution :: Int -> ClickhouseExecution -> ClickhouseExecution
retryExecution retries normal =
  normal
    { executeRequest = \connection request action ->
        recovering
          (constantDelay 0 <> limitRetries retries)
          [const $ Handler \(_ :: TransientFailure) -> pure True]
          (\_ -> executeRequest normal connection request action)
    }

response :: [Integer] -> ByteString
response values =
  BSL.toStrict (toLazyByteString $ encodeLEB128 1 <> encodeValue (ClickString "value") <> encodeValue (ClickString "UInt64"))
    <> encodeRows (Vector.toList $ rows values)

rows :: [Integer] -> Vector (Vector ClickhouseType)
rows = Vector.fromList . map (Vector.singleton . ClickUInt64 . fromInteger)

append :: IORef [value] -> value -> IO ()
append reference value = atomicModifyIORef' reference (\values -> (values <> [value], ()))

increment :: IORef Int -> IO Int
increment reference = atomicModifyIORef' reference (\count -> (count + 1, count + 1))

assertEqual :: (Eq value, Show value) => String -> value -> value -> IO ()
assertEqual label expected actual =
  unless (expected == actual) $ fail (label <> ": expected " <> show expected <> ", got " <> show actual)

assertConnectionFields :: ClickhouseConnectionSettings client -> ClickhouseConnectionSettings other -> IO ()
assertConnectionFields expected actual = do
  assertEqual "username" (username expected) (username actual)
  assertEqual "password" (password expected) (password actual)
  assertEqual "database" (database expected) (database actual)
  assertEqual "settings" (settings expected) (settings actual)
  assertEqual "extraHeaders" (extraHeaders expected) (extraHeaders actual)

assertRequests :: [CHRequest] -> [CHRequest] -> IO ()
assertRequests expected actual = do
  assertEqual "request count" (length expected) (length actual)
  mapM_ compareRequest (zip expected actual)
 where
  compareRequest (wanted, received) = do
    assertEqual "request SQL" (requestSql wanted) (requestSql received)
    assertEqual "request data" (requestData wanted) (requestData received)
    assertEqual "request params" (requestParams wanted) (requestParams received)
    assertEqual "typed query params" (requestQueryParams wanted) (requestQueryParams received)
    assertEqual "response format" (requestResponseFormat wanted) (requestResponseFormat received)
    let externalFields table = (externalTableName table, externalColumns table, externalRows table)
    assertEqual "external tables" (map externalFields $ requestExternals wanted) (map externalFields $ requestExternals received)
