{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module HTTPControlSpec (httpControlChecks) where

import Control.Concurrent (forkFinally, killThread)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar, tryPutMVar)
import Control.Exception (AsyncException (ThreadKilled), SomeException, bracket, displayException, finally, fromException, throwIO, try)
import Control.Monad (unless, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Data.ByteString qualified as BS
import Data.Conduit (catchC, runConduit, (.|))
import Data.Conduit.Combinators qualified as Conduit
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (isInfixOf)
import Database.ClickHouse hiding (runRequest)
import Database.Clickhouse.Client.HTTP.Client (buildRequest)
import Database.Clickhouse.Client.HTTP.Client qualified as Legacy (ClickhouseHTTPTransport (..))
import HCurl.Metrics qualified as CurlMetrics
import HCurl.Request qualified as Curl
import HCurl.Response qualified as HCurlResponse
import HCurl.Types qualified as Curl
import HTTPMetricsSpec
import Network.Socket (Family (AF_INET), SockAddr (SockAddrInet), Socket, SocketOption (Linger, RecvBuffer), SocketType (Stream), StructLinger (..), bind, close, defaultProtocol, getSocketName, setSockOpt, setSocketOption, socket, tupleToHostAddress)
import Network.Socket.ByteString qualified as SocketBS
import System.Timeout (timeout)

httpControlChecks :: [(String, IO (Either String ()))]
httpControlChecks =
  [ check "request hook and native options reach the HTTP server" requestConfiguration
  , check "request-body override is reflected in final metrics" requestBodyOverride
  , check "native timeout override is applied after connection defaults" timeoutOverride
  , check "response hook observes headers before body consumption" responseHook
  , check "custom response policy accepts HTTP 503" acceptServerStatus
  , check "custom response policy rejects HTTP 200" rejectSuccessStatus
  , check "error-body cap retains a prefix while draining the response" boundedErrorBody
  , check "zero error-body cap preserves status and metrics" emptyErrorBody
  , check "success events are ordered, correlated and contain final metrics" successEvents
  , check "HTTP error event matches exception metrics" serverFailureEvents
  , check "disconnect before headers emits terminal metrics" disconnectEvents
  , check "connection refusal emits zero-byte terminal metrics" refusalEvents
  , check "truncated error response emits one transport failure event" truncatedEvents
  , check "reset upload event includes partially uploaded byte counts" resetEvents
  , check "prefix cancellation emits metrics and preserves agent reuse" cancellationEvents
  , check "asynchronous cancellation before headers emits metrics" interruptedEvents
  , check "request hook exceptions prevent submission" requestHookFailure
  , check "response hook exceptions cancel and preserve agent reuse" responseHookFailure
  , check "synchronous logger exceptions do not change request outcomes" loggerFailure
  , check "asynchronous logger exceptions are not swallowed" asynchronousLoggerFailure
  , check "invalid response configuration prevents submission" invalidConfiguration
  , check "legacy transport construction and record updates preserve configuration" transportCompatibility
  , check "parallel requests keep separate ordered event streams" concurrentEvents
  ]
 where
  check name action =
    ( name
    , do
        result <- try @SomeException $ timeout 5_000_000 action
        pure $ case result of
          Left exception -> Left (displayException exception)
          Right Nothing -> Left "local HTTP control check timed out"
          Right (Just ()) -> Right ()
    )

requestConfiguration :: IO ()
requestConfiguration = do
  captured <- newEmptyMVar
  hookCalls <- newIORef (0 :: Int)
  withLocalServerUsing
    2_000
    ( \client -> do
        (headers, buffered) <- receiveHeaders client BS.empty
        putMVar captured headers
        body <- receiveBody client (headers <> "\r\n\r\n" <> buffered)
        SocketBS.sendAll client "HTTP/1.1 200 OK\r\nContent-Length: 2\r\nConnection: close\r\n\r\nok"
        pure (BS.length body)
    )
    \connection receivedBytes -> do
      let config =
            defaultHTTPConfig
              { httpExtraOptions = [OptionAcceptEncoding "identity"]
              , httpStreamConfig = StreamConfig 1
              , httpModifyRequest = \request native -> do
                  unless (not $ null $ requestQueryParams request) $ fail "hook lost typed query parameters"
                  case Curl.body native of
                    Curl.Buffer body -> unless ("name=\"query\"" `BS.isInfixOf` body) $ fail "hook ran before multipart serialization"
                    _ -> fail "hook did not receive the multipart body"
                  atomicModifyIORef' hookCalls (\count -> (count + 1, ()))
                  case Curl.headers native of
                    Curl.HeaderList headers -> pure native {Curl.headers = Curl.HeaderList (headers <> ["X-Driver-Hook: configured"])}
                    _ -> fail "hook lost the driver's authentication headers"
              }
      runRequest (configure config connection) >>= either throwIO pure
      _ <- receivedBytes
      headers <- takeMVar captured
      unless ("Accept-Encoding: identity" `BS.isInfixOf` headers) $ fail "native encoding option was not applied"
      unless ("X-Driver-Hook: configured" `BS.isInfixOf` headers) $ fail "request hook was not applied"
      readIORef hookCalls >>= \count -> unless (count == 1) $ fail "request hook ran more than once"

responseHook :: IO ()
responseHook = do
  observed <- newEmptyMVar
  releaseBody <- newEmptyMVar
  withLocalServer
    ( \client -> do
        SocketBS.sendAll client "HTTP/1.1 200 OK\r\nX-Driver-Test: observed\r\nContent-Length: 2\r\nConnection: close\r\n\r\n"
        takeMVar releaseBody
        SocketBS.sendAll client "ok"
    )
    \connection _ -> do
      let config =
            defaultHTTPConfig
              { httpOnResponse = \request response -> do
                  unless ("driver-secret-query" `BS.isInfixOf` requestSql request) $ fail "response hook lost request context"
                  putMVar observed (lookup "X-Driver-Test" (responseHeaders response))
                  putMVar releaseBody ()
              }
      runRequest (configure config connection) >>= either throwIO pure
      takeMVar observed >>= \value -> unless (value == Just "observed") $ fail "response header was not available"
 where
  responseHeaders = HCurlResponse.headers

requestBodyOverride :: IO ()
requestBodyOverride = do
  events <- newIORef []
  let payload = "SELECT number FROM numbers(123)"
  withLocalServerUsing
    2_000
    ( \client -> do
        body <- receiveBody client BS.empty
        unless (body == payload) $ fail "request-body override was not sent"
        SocketBS.sendAll client "HTTP/1.1 200 OK\r\nContent-Length: 2\r\nConnection: close\r\n\r\nok"
        pure (BS.length body)
    )
    \connection _ -> do
      let config = (eventConfig events) {httpModifyRequest = \_ native -> pure native {Curl.body = Curl.Buffer payload}}
      runResourceT $ runConduit $ sendSource (configure config connection) (commandRequest "SELECT 1") .| Conduit.sinkNull
      recorded <- readEvents events
      let sizes = [size | HTTPRequestStarted _ size <- recorded]
          metrics = [snapshot | HTTPRequestFinished _ _ snapshot <- recorded]
          expected = fromIntegral (BS.length payload)
      unless (sizes == [expected] && map requestBodyBytes metrics == [expected]) $ fail "events used the original body size"
      unless (map (CurlMetrics.uploadProgress . curlMetrics) metrics == [expected]) $ fail "upload metrics used the original body size"

timeoutOverride :: IO ()
timeoutOverride = do
  releaseServer <- newEmptyMVar
  events <- newIORef []
  withLocalServer (\_ -> takeMVar releaseServer) \connection _ -> do
    let config = (eventConfig events) {httpExtraOptions = [OptionTimeoutMs 200]}
    result <- runRequest (configure config connection) `finally` void (tryPutMVar releaseServer ())
    case result of
      Left exception
        | Just transportError <- fromException @ClickhouseTransportException exception ->
            unless (transportMessage transportError == "OperationTimedout") $ fail "native timeout override was ignored"
      _ -> fail "expected the overridden native timeout"
    assertLifecycle events Nothing HTTPTransportFailed "OperationTimedout"
    recorded <- readEvents events
    unless (all ((< 1_000_000) . CurlMetrics.totalTime . curlMetrics) [metrics | HTTPRequestFinished _ _ metrics <- recorded]) $ fail "the original two-second timeout was used"

acceptServerStatus :: IO ()
acceptServerStatus = withResponseServer "503 Service Unavailable" 2 "ok" \connection _ -> do
  events <- newIORef []
  let config = (eventConfig events) {httpIsErrorStatus = const False}
  runRequest (configure config connection) >>= either throwIO pure
  assertLifecycle events (Just 503) HTTPTransferSucceeded "Ok"

rejectSuccessStatus :: IO ()
rejectSuccessStatus = withResponseServer "200 OK" 2 "ok" \connection _ -> do
  events <- newIORef []
  let config = (eventConfig events) {httpIsErrorStatus = const True}
  result <- runRequest (configure config connection)
  case result of
    Left exception
      | Just serverError <- fromException @ClickhouseServerException exception ->
          unless (serverStatus serverError == 200 && serverMessage serverError == "ok") $ fail "custom status policy was ignored"
    _ -> fail "expected a server exception for the rejected status"
  assertLifecycle events (Just 200) HTTPResponseFailed "Ok"

boundedErrorBody :: IO ()
boundedErrorBody = errorBodyLimit 7

emptyErrorBody :: IO ()
emptyErrorBody = errorBodyLimit 0

errorBodyLimit :: Int -> IO ()
errorBodyLimit limit = do
  let payload = BS.replicate 262_144 120
  withResponseServer "503 Service Unavailable" (BS.length payload) payload \connection receivedBytes -> do
    let config = defaultHTTPConfig {httpErrorBodyLimit = limit, httpStreamConfig = StreamConfig 1}
    result <- runRequest (configure config connection)
    _ <- receivedBytes
    case result of
      Left exception
        | Just serverError <- fromException @ClickhouseServerException exception
        , Just metrics <- serverMetrics serverError -> do
            unless (serverMessage serverError == BS.take limit payload) $ fail "error-body cap was ignored"
            unless (CurlMetrics.downloadProgress (curlMetrics metrics) == fromIntegral (BS.length payload)) $ fail "capped response was not fully drained"
      _ -> fail "expected HTTP 503 with metrics"

successEvents :: IO ()
successEvents = withResponseServer "200 OK" 2 "ok" \connection receivedBytes -> do
  events <- newIORef []
  runRequest (configure (eventConfig events) connection) >>= either throwIO pure
  bytes <- receivedBytes
  assertLifecycle events (Just 200) HTTPTransferSucceeded "Ok"
  readEvents events >>= \recorded -> do
    let metrics = [snapshot | HTTPRequestFinished _ _ snapshot <- recorded]
    unless (map requestBodyBytes metrics == [fromIntegral bytes]) $ fail "terminal event lost the actual request size"
    unless (map (CurlMetrics.downloadProgress . curlMetrics) metrics == [2]) $ fail "terminal event did not wait for completion"
    mapM_
      (\secret -> unless (not $ secret `isInfixOf` show recorded) $ fail "automatic events leaked request secrets")
      ["driver-secret-query", "driver-secret-param", "driver-secret-user", "driver-secret-password"]

serverFailureEvents :: IO ()
serverFailureEvents = withResponseServer "503 Service Unavailable" 2 "no" \connection _ -> do
  events <- newIORef []
  result <- runRequest (configure (eventConfig events) connection)
  assertLifecycle events (Just 503) HTTPResponseFailed "Ok"
  case result of
    Left exception | Just serverError <- fromException @ClickhouseServerException exception -> do
      recorded <- readEvents events
      unless ([Just metrics | HTTPRequestFinished _ _ metrics <- recorded] == [serverMetrics serverError]) $ fail "event and exception snapshots differ"
    _ -> fail "expected an HTTP error"

disconnectEvents :: IO ()
disconnectEvents = withLocalServer (\_ -> pure ()) \connection _ -> do
  events <- newIORef []
  result <- runRequest (configure (eventConfig events) connection)
  case result of
    Left exception
      | Just transportError <- fromException @ClickhouseTransportException exception ->
          assertLifecycle events Nothing HTTPTransportFailed (transportMessage transportError)
    _ -> fail "expected a disconnect before headers"

refusalEvents :: IO ()
refusalEvents = bracket (socket AF_INET Stream defaultProtocol) close \reserved -> do
  bind reserved (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
  SockAddrInet serverPort _ <- getSocketName reserved
  withConnection (fromIntegral serverPort) 1_000 \connection -> do
    events <- newIORef []
    result <- runRequest (configure (eventConfig events) connection)
    case result of
      Left exception | Just _ <- fromException @ClickhouseTransportException exception -> pure ()
      _ -> fail "expected a refused connection"
    assertLifecycle events Nothing HTTPTransportFailed "CouldntConnect"
    recorded <- readEvents events
    unless (all ((== 0) . CurlMetrics.uploadProgress . curlMetrics) [metrics | HTTPRequestFinished _ _ metrics <- recorded]) $ fail "connection refusal reported uploaded bytes"

truncatedEvents :: IO ()
truncatedEvents = withResponseServer "503 Service Unavailable" 100 "partial" \connection _ -> do
  events <- newIORef []
  result <- runRequest (configure (eventConfig events) connection)
  case result of
    Left exception | Just _ <- fromException @ClickhouseTransportException exception -> pure ()
    _ -> fail "expected a truncated response"
  assertLifecycle events (Just 503) HTTPTransportFailed "PartialFile"

resetEvents :: IO ()
resetEvents = withLocalServerUsing 2_000 receivePrefix \connection receivedBytes -> do
  events <- newIORef []
  let request = selectRequestWithParams [("value", ClickString $ BS.replicate 8_388_608 120)] "SELECT {value:String}"
  result <- try @SomeException $ runResourceT $ runConduit $ sendSource (configure (eventConfig events) connection) request .| Conduit.sinkNull
  actualReceived <- receivedBytes
  case result of
    Left exception | Just transportError <- fromException @ClickhouseTransportException exception -> do
      recorded <- readEvents events
      case [metrics | HTTPRequestFinished _ HTTPTransportFailed metrics <- recorded] of
        [metrics] -> do
          let uploaded = CurlMetrics.uploadProgress (curlMetrics metrics)
          unless (uploaded >= fromIntegral actualReceived && uploaded < requestBodyBytes metrics) $ fail "reset event lost partial upload metrics"
          unless (Just metrics == transportMetrics transportError) $ fail "reset event and exception snapshots differ"
        _ -> fail "reset did not produce one terminal failure event"
    _ -> fail "expected an upload reset"
 where
  receivePrefix client = do
    setSocketOption client RecvBuffer 1_024
    (_, buffered) <- receiveHeaders client BS.empty
    SocketBS.sendAll client "HTTP/1.1 100 Continue\r\n\r\n"
    prefix <- if BS.null buffered then SocketBS.recv client 256 else pure buffered
    setSockOpt client Linger (StructLinger 1 0)
    pure (BS.length prefix)

cancellationEvents :: IO ()
cancellationEvents = do
  releaseServer <- newEmptyMVar
  events <- newIORef []
  let partialReply client = do
        _ <- receiveBody client BS.empty
        SocketBS.sendAll client "HTTP/1.1 200 OK\r\nContent-Length: 100\r\n\r\nok"
        takeMVar releaseServer
  withLocalServers 2_000 [partialReply, successfulReply] \connection -> do
    let configured = configure (eventConfig events) connection
    finally
      (runResourceT $ runConduit $ sendSource configured (commandRequest "SELECT 1") .| Conduit.take 1 .| Conduit.sinkNull)
      (void $ tryPutMVar releaseServer ())
    recorded <- readEvents events
    case [metrics | HTTPRequestFinished _ HTTPTransferCancelled metrics <- recorded] of
      [metrics] -> unless (responseStatus metrics == Just 200) $ fail "cancellation lost the response status"
      _ -> fail "prefix cancellation did not produce one cancellation event"
    runRequest configured >>= either throwIO pure
    allEvents <- readEvents events
    unless (length [() | HTTPRequestFinished {} <- allEvents] == 2) $ fail "agent reuse duplicated or lost terminal events"
    unless (length [() | HTTPRequestFinished _ HTTPTransferSucceeded _ <- allEvents] == 1) $ fail "agent reuse failed"

interruptedEvents :: IO ()
interruptedEvents = do
  requestArrived <- newEmptyMVar
  releaseServer <- newEmptyMVar
  events <- newIORef []
  withLocalServerUsing
    2_000
    ( \client -> do
        body <- receiveBody client BS.empty
        putMVar requestArrived ()
        takeMVar releaseServer
        pure (BS.length body)
    )
    \connection _ -> do
      result <- newEmptyMVar
      worker <- forkFinally (runRequest $ configure (eventConfig events) connection) (putMVar result)
      finally
        ( do
            takeMVar requestArrived
            killThread worker
            takeMVar result >>= \case
              Right (Left exception) | Just ThreadKilled <- fromException exception -> pure ()
              other -> fail ("asynchronous cancellation was swallowed: " <> show other)
            recorded <- readEvents events
            case recorded of
              [HTTPRequestStarted identifier _, HTTPRequestFinished finished HTTPTransferCancelled metrics]
                | identifier == finished && responseStatus metrics == Nothing -> pure ()
              _ -> fail "pre-header cancellation lost its terminal event"
        )
        (void $ tryPutMVar releaseServer ())

requestHookFailure :: IO ()
requestHookFailure = withConnection 1 1_000 \connection -> do
  events <- newIORef []
  let config = (eventConfig events) {httpModifyRequest = \_ _ -> fail "request-policy-failure"}
  result <- runRequest (configure config connection)
  assertException "request-policy-failure" result
  readEvents events >>= \recorded -> unless (null recorded) $ fail "a rejected request was submitted"

responseHookFailure :: IO ()
responseHookFailure = withLocalServers 2_000 [successfulReply, successfulReply] \connection -> do
  events <- newIORef []
  let config = (eventConfig events) {httpOnResponse = \_ _ -> fail "response-policy-failure"}
  runResourceT do
    runConduit $
      (sendSource (configure config connection) (commandRequest "SELECT 1") `catchC` \(exception :: SomeException) -> liftIO $ assertException "response-policy-failure" (Left exception))
        .| Conduit.sinkNull
    recorded <- liftIO $ readEvents events
    liftIO $ unless (length [() | HTTPRequestFinished _ HTTPTransferCancelled _ <- recorded] == 1) $ fail "response hook failure deferred cancellation until scope exit"
    runConduit $ sendSource connection (commandRequest "SELECT 1") .| Conduit.sinkNull

loggerFailure :: IO ()
loggerFailure = do
  let config = defaultHTTPConfig {httpOnEvent = \_ -> fail "logger-failure"}
  withResponseServer "200 OK" 2 "ok" \connection _ -> runRequest (configure config connection) >>= either throwIO pure
  withResponseServer "503 Service Unavailable" 2 "no" \connection _ -> do
    result <- runRequest (configure config connection)
    case result of
      Left exception
        | Just serverError <- fromException @ClickhouseServerException exception ->
            unless (serverStatus serverError == 503 && serverMetrics serverError /= Nothing) $ fail "logger failure changed the server exception"
      _ -> fail "logger failure replaced the server exception"

asynchronousLoggerFailure :: IO ()
asynchronousLoggerFailure = withResponseServer "200 OK" 2 "ok" \connection _ -> do
  events <- newIORef []
  let record = httpOnEvent (eventConfig events)
      config =
        defaultHTTPConfig
          { httpOnEvent = \event -> do
              record event
              case event of
                HTTPResponseReceived {} -> throwIO ThreadKilled
                _ -> pure ()
          }
  result <- runRequest (configure config connection)
  case result of
    Left exception | Just ThreadKilled <- fromException exception -> pure ()
    _ -> fail "asynchronous logger exception was swallowed"
  recorded <- readEvents events
  unless (length [() | HTTPRequestFinished _ HTTPTransferCancelled _ <- recorded] == 1) $ fail "asynchronous logger failure leaked a transfer"

invalidConfiguration :: IO ()
invalidConfiguration = withConnection 1 1_000 \connection -> do
  events <- newIORef []
  let configs =
        [ (eventConfig events) {httpErrorBodyLimit = -1}
        , (eventConfig events) {httpStreamConfig = StreamConfig 0}
        ]
  mapM_
    ( \config ->
        runRequest (configure config connection) >>= \case
          Left _ -> pure ()
          Right () -> fail "invalid configuration was accepted"
    )
    configs
  readEvents events >>= \recorded -> unless (null recorded) $ fail "invalid configuration submitted a request"

transportCompatibility :: IO ()
transportCompatibility = withConnection 1 1_000 \connection -> do
  let agent = transportAgent (connectionSettings connection)
      positional = Legacy.ClickhouseHTTPTransport defaultHTTPSettings agent
      named = Legacy.ClickhouseHTTPTransport {Legacy.transportOptions = defaultHTTPSettings, Legacy.transportAgent = agent}
      config = defaultHTTPConfig {httpErrorBodyLimit = 11, httpExtraOptions = [OptionAcceptEncoding "identity"]}
      updated = (withHTTPConfig config positional) {transportOptions = defaultHTTPSettings {responseTimeoutMS = 23}}
  unless (httpErrorBodyLimit (transportConfig named) == 4096) $ fail "legacy record construction lost defaults"
  unless (httpErrorBodyLimit (transportConfig updated) == 11) $ fail "record update lost HTTP configuration"
  native <- buildRequest (defaultConnection updated) (commandRequest "SELECT 1")
  unless (Curl.timeoutMS native == 23 && Curl.extraOptions native == [OptionAcceptEncoding "identity"]) $ fail "record update did not preserve request configuration"

concurrentEvents :: IO ()
concurrentEvents = withLocalServers 2_000 [successfulReply, successfulReply] \connection -> do
  events <- newIORef []
  completed <- newEmptyMVar
  let configured = configure (eventConfig events) connection
  workers <- mapM (\_ -> forkFinally (runRequest configured) (putMVar completed)) [1 :: Int, 2]
  finally
    ( do
        mapM_ (\_ -> takeMVar completed >>= either throwIO (either throwIO pure)) workers
        recorded <- readEvents events
        let identifiers = [identifier | HTTPRequestStarted identifier _ <- recorded]
        case identifiers of
          [first, second] | first /= second -> pure ()
          _ -> fail "parallel transfers did not have distinct identifiers"
        mapM_
          ( \identifier -> do
              perRequest <- newIORef $ reverse $ filter ((== identifier) . eventRequestId) recorded
              assertLifecycle perRequest (Just 200) HTTPTransferSucceeded "Ok"
          )
          identifiers
    )
    (mapM_ killThread workers)

configure :: ClickhouseHTTPConfig -> ClickhouseConnectionSettings ClientHTTP -> ClickhouseConnectionSettings ClientHTTP
configure config connection = connection {connectionSettings = withHTTPConfig config (connectionSettings connection)}

eventConfig :: IORef [ClickhouseHTTPEvent] -> ClickhouseHTTPConfig
eventConfig events =
  defaultHTTPConfig
    { httpOnEvent = \event -> atomicModifyIORef' events (\recorded -> (event : recorded, ()))
    }

readEvents :: IORef [ClickhouseHTTPEvent] -> IO [ClickhouseHTTPEvent]
readEvents events = reverse <$> readIORef events

assertLifecycle :: IORef [ClickhouseHTTPEvent] -> Maybe Int -> ClickhouseHTTPOutcome -> String -> IO ()
assertLifecycle events status expected code = do
  recorded <- readEvents events
  case (status, recorded) of
    (Nothing, [HTTPRequestStarted identifier _, HTTPRequestFinished finished outcome metrics]) -> validate identifier finished outcome metrics
    (Just expectedStatus, [HTTPRequestStarted identifier _, HTTPResponseReceived received actualStatus, HTTPRequestFinished finished outcome metrics])
      | received == identifier && expectedStatus == actualStatus -> validate identifier finished outcome metrics
    _ -> fail ("unexpected event order: " <> show recorded)
 where
  validate identifier finished outcome metrics =
    unless (identifier == finished && outcome == expected && responseStatus metrics == status && transferCode metrics == code) $
      fail ("unexpected terminal event: " <> show (identifier, finished, outcome, metrics))

assertException :: String -> Either SomeException result -> IO ()
assertException expected = \case
  Left exception | expected `isInfixOf` displayException exception -> pure ()
  _ -> fail ("expected exception: " <> expected)

successfulReply :: Socket -> IO ()
successfulReply client = do
  _ <- receiveBody client BS.empty
  SocketBS.sendAll client "HTTP/1.1 200 OK\r\nContent-Length: 2\r\nConnection: close\r\n\r\nok"
