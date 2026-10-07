{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module HTTPMetricsSpec
  ( httpMetricsChecks
  , runRequest
  , withResponseServer
  , withLocalServer
  , withLocalServerUsing
  , withLocalServers
  , withConnection
  , receiveBody
  , receiveHeaders
  ) where

import Control.Concurrent (forkFinally, killThread)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, bracket, displayException, finally, fromException, throwIO, try)
import Control.Monad (unless, when)
import Control.Monad.Trans.Resource (runResourceT)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.Conduit (runConduit, (.|))
import Data.Conduit.Combinators (sinkNull)
import Data.Conduit.Combinators qualified as Conduit
import Data.List (isInfixOf)
import Database.ClickHouse hiding (runRequest)
import Database.Clickhouse.Client.Types qualified as Legacy (ClickhouseServerException (..), ClickhouseTransportException (..))
import HCurl.Agent qualified as CurlAgent
import HCurl.Metrics qualified as Curl
import Network.Socket
import Network.Socket.ByteString qualified as SocketBS
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)
import System.Mem (performGC)
import System.Timeout (timeout)

httpMetricsChecks :: [(String, IO (Either String ()))]
httpMetricsChecks =
  [ check "HTTP 503 exceptions include completed upload metrics" serverErrorMetrics
  , check "disconnect before response headers includes upload metrics" disconnectedMetrics
  , check "TCP reset during upload includes partial upload metrics" resetUploadMetrics
  , check "truncated response includes transfer metrics" truncatedMetrics
  , check "truncated HTTP 503 keeps its status in transfer metrics" truncatedErrorMetrics
  , check "connection refusal includes zero-byte transfer metrics" refusedMetrics
  , check "timeout before response headers includes transfer metrics" timeoutMetrics
  , check "successful response remains readable" successfulResponse
  , check "agent remains usable after a reset with metrics" reuseAfterReset
  , check "early response cancellation keeps the agent usable" reuseAfterCancellation
  , check "legacy exception constructors and patterns remain compatible" legacyExceptions
  ]
 where
  check name action =
    ( name
    , do
        outcome <- try @SomeException $ timeout 5_000_000 action
        pure $ case outcome of
          Left exception -> Left (displayException exception)
          Right Nothing -> Left "local HTTP metrics check timed out"
          Right (Just ()) -> Right ()
    )

serverErrorMetrics :: IO ()
serverErrorMetrics = withResponseServer "503 Service Unavailable" 23 "Code: 209. read timeout" \connection receivedBytes -> do
  outcome <- runRequest connection
  bodyLength <- receivedBytes
  case outcome of
    Left exception | Just serverError <- fromException @ClickhouseServerException exception -> do
      unless (serverStatus serverError == 503) $ fail "wrong HTTP error status"
      assertLoggedMetrics bodyLength exception
      assertSnapshot bodyLength (Just 503) 23 "Ok" (serverMetrics serverError)
    _ -> fail "expected ClickhouseServerException"

disconnectedMetrics :: IO ()
disconnectedMetrics = withLocalServer (\connection -> setSockOpt connection Linger (StructLinger 1 0)) \connection receivedBytes -> do
  outcome <- runRequest connection
  bodyLength <- receivedBytes
  case outcome of
    Left exception | Just transportError <- fromException @ClickhouseTransportException exception -> do
      assertLoggedMetrics bodyLength exception
      assertSnapshot bodyLength Nothing 0 (transportMessage transportError) (transportMetrics transportError)
    _ -> fail "expected ClickhouseTransportException before response headers"

resetUploadMetrics :: IO ()
resetUploadMetrics = withLocalServerUsing 2_000 receivePrefix \connection receivedBytes -> do
  let request =
        selectRequestWithParams
          [("value", ClickString $ BS.replicate 8_388_608 120)]
          "SELECT 'driver-secret-query', {value:String}"
  outcome <- try @SomeException $ runResourceT $ runConduit $ sendSource connection request .| sinkNull
  actualReceived <- receivedBytes
  case outcome of
    Left exception
      | Just transportError <- fromException @ClickhouseTransportException exception
      , Just metrics <- transportMetrics transportError -> do
          let uploaded = Curl.uploadProgress (curlMetrics metrics)
          unless (requestBodyBytes metrics > 8_388_608) $ fail "large multipart body was not advertised"
          unless (uploaded >= fromIntegral actualReceived && uploaded < requestBodyBytes metrics) $ fail "reset did not interrupt the upload"
          unless (responseStatus metrics == Just 100) $ fail "reset lost the informational response status"
          unless (transferCode metrics == transportMessage transportError) $ fail "reset curl code was lost"
          unless ("uploadProgress = " `isInfixOf` displayException exception) $ fail "partial metrics were not logged"
          recordEvidence exception
    _ -> fail "expected a reset with partial upload metrics"
 where
  receivePrefix connection = do
    setSocketOption connection RecvBuffer 1_024
    (_, initialBody) <- receiveHeaders connection BS.empty
    SocketBS.sendAll connection "HTTP/1.1 100 Continue\r\n\r\n"
    bodyPrefix <- if BS.null initialBody then SocketBS.recv connection 256 else pure initialBody
    setSockOpt connection Linger (StructLinger 1 0)
    pure (BS.length bodyPrefix)

truncatedMetrics :: IO ()
truncatedMetrics = withResponseServer "200 OK" 100 "partial" \connection receivedBytes -> do
  outcome <- runRequest connection
  bodyLength <- receivedBytes
  case outcome of
    Left exception | Just transportError <- fromException @ClickhouseTransportException exception -> do
      assertLoggedMetrics bodyLength exception
      assertSnapshot bodyLength (Just 200) 7 "PartialFile" (transportMetrics transportError)
    _ -> fail "expected ClickhouseTransportException for a truncated body"

truncatedErrorMetrics :: IO ()
truncatedErrorMetrics = withResponseServer "503 Service Unavailable" 100 "partial" \connection receivedBytes -> do
  outcome <- runRequest connection
  bodyLength <- receivedBytes
  case outcome of
    Left exception | Just transportError <- fromException @ClickhouseTransportException exception -> do
      assertLoggedMetrics bodyLength exception
      assertSnapshot bodyLength (Just 503) 7 "PartialFile" (transportMetrics transportError)
    _ -> fail "expected a transport error while draining HTTP 503"

refusedMetrics :: IO ()
refusedMetrics = bracket (socket AF_INET Stream defaultProtocol) close \reserved -> do
  bind reserved (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
  SockAddrInet serverPort _ <- getSocketName reserved
  withConnection (fromIntegral serverPort) 1_000 \connection -> do
    runRequest connection >>= \case
      Left exception
        | Just transportError <- fromException @ClickhouseTransportException exception
        , Just metrics <- transportMetrics transportError -> do
            unless (Curl.uploadProgress (curlMetrics metrics) == 0) $ fail "refused connection reported sent bytes"
            unless (requestBodyBytes metrics > 0 && responseStatus metrics == Nothing) $ fail "refused request metadata was lost"
            unless (transferCode metrics == "CouldntConnect") $ fail "connection refusal code was lost"
            unless ("uploadProgress = 0" `isInfixOf` displayException exception) $ fail "refused metrics were not logged"
            recordEvidence exception
      _ -> fail "expected a refusal with metrics"

timeoutMetrics :: IO ()
timeoutMetrics = withLocalServerUsing 200 waitForTimeout responder
 where
  waitForTimeout connection = do
    body <- receiveBody connection BS.empty
    _ <- SocketBS.recv connection 1
    pure (BS.length body)
  responder connection receivedBytes = do
    outcome <- runRequest connection
    bodyLength <- receivedBytes
    case outcome of
      Left exception | Just transportError <- fromException @ClickhouseTransportException exception -> do
        assertLoggedMetrics bodyLength exception
        assertSnapshot bodyLength Nothing 0 "OperationTimedout" (transportMetrics transportError)
      _ -> fail "expected a timeout with metrics"

legacyExceptions :: IO ()
legacyExceptions = do
  let transport = Legacy.ClickhouseTransportException {Legacy.transportMessage = "legacy"}
      server = Legacy.ClickhouseServerException {Legacy.serverStatus = 503, Legacy.serverMessage = "legacy"}
  unless (transportMetrics transport == Nothing && serverMetrics server == Nothing) $ fail "legacy constructors unexpectedly have metrics"
  unless (show transport == "ClickhouseTransportException {transportMessage = \"legacy\"}") $ fail "legacy transport formatting changed"
  unless (show server == "ClickhouseServerException {serverStatus = 503, serverMessage = \"legacy\"}") $ fail "legacy server formatting changed"
  unless (show (Just transport) == "Just (ClickhouseTransportException {transportMessage = \"legacy\"})") $ fail "transport Show precedence changed"
  unless (show (Just server) == "Just (ClickhouseServerException {serverStatus = 503, serverMessage = \"legacy\"})") $ fail "server Show precedence changed"
  case (transport, server) of
    (Legacy.ClickhouseTransportException "legacy", Legacy.ClickhouseServerException 503 "legacy") -> pure ()
    _ -> fail "legacy constructor patterns stopped matching"

successfulResponse :: IO ()
successfulResponse = withResponseServer "200 OK" 2 "ok" \connection receivedBytes -> do
  runRequest connection >>= either throwIO pure
  bodyLength <- receivedBytes
  unless (bodyLength > 0) $ fail "successful request did not send a body"

reuseAfterReset :: IO ()
reuseAfterReset = withLocalServers 2_000 [resetResponse, successfulReply] \connection -> do
  exception <-
    runRequest connection >>= \case
      Left failure | Just transportError <- fromException @ClickhouseTransportException failure -> do
        case transportError of
          ClickhouseTransportException message ->
            unless (message == transportMessage transportError) $ fail "legacy pattern lost the transport message"
        unless (transportMetrics transportError /= Nothing) $ fail "reset did not retain its snapshot"
        pure failure
      _ -> fail "expected a reset before reusing the agent"
  performGC
  runRequest connection >>= either throwIO pure
  unless ("transportMetrics = Just" `isInfixOf` displayException exception) $ fail "reset snapshot was lost after reuse"
 where
  resetResponse client = do
    _ <- receiveBody client BS.empty
    setSockOpt client Linger (StructLinger 1 0)

reuseAfterCancellation :: IO ()
reuseAfterCancellation = withLocalServers 2_000 [partialReply, successfulReply] \connection -> do
  runResourceT $ runConduit $ sendSource connection (selectRequest "SELECT 1") .| Conduit.take 1 .| sinkNull
  runRequest connection >>= either throwIO pure
 where
  partialReply client = do
    _ <- receiveBody client BS.empty
    SocketBS.sendAll client "HTTP/1.1 200 OK\r\nContent-Length: 1048576\r\n\r\nx"
    remaining <- SocketBS.recv client 1
    unless (BS.null remaining) $ fail "cancelled response connection did not close"

successfulReply :: Socket -> IO ()
successfulReply client = do
  _ <- receiveBody client BS.empty
  SocketBS.sendAll client "HTTP/1.1 200 OK\r\nContent-Length: 2\r\nConnection: close\r\n\r\nok"

runRequest :: ClickhouseConnectionSettings ClientHTTP -> IO (Either SomeException ())
runRequest connection =
  try $
    runResourceT $
      runConduit $
        sendSource
          connection
          ( selectRequestWithParams
              [("value", ClickString "driver-secret-param")]
              "SELECT 'driver-secret-query', {value:String}"
          )
          .| sinkNull

assertSnapshot :: Int -> Maybe Int -> Int -> String -> Maybe ClickhouseTransferMetrics -> IO ()
assertSnapshot expectedUpload status expectedDownload code = \case
  Nothing -> fail "typed transfer metrics were not attached"
  Just metrics -> do
    unless (requestBodyBytes metrics == fromIntegral expectedUpload) $ fail "request body size differs from server evidence"
    unless (responseStatus metrics == status && transferCode metrics == code) $ fail "status or curl code was lost"
    unless (Curl.uploadProgress (curlMetrics metrics) == fromIntegral expectedUpload) $ fail "uploaded byte count differs from server evidence"
    unless (Curl.downloadProgress (curlMetrics metrics) == fromIntegral expectedDownload) $ fail "downloaded byte count differs from server evidence"
    unless (Curl.totalTime (curlMetrics metrics) >= 0) $ fail "invalid transfer time"

assertLoggedMetrics :: Int -> SomeException -> IO ()
assertLoggedMetrics receivedBytes exception = do
  let logged = displayException exception
  mapM_
    (\expected -> unless (expected `isInfixOf` logged) $ fail ("missing diagnostic: " <> expected <> "; got " <> logged))
    [ "requestBodyBytes = " <> show receivedBytes
    , "uploadProgress = " <> show receivedBytes
    , "uploadTotal = " <> show receivedBytes
    , "totalTime = "
    , "connectTime = "
    ]
  mapM_
    (\secret -> unless (not $ secret `isInfixOf` logged) $ fail "request data leaked into metrics")
    ["driver-secret-query", "driver-secret-param", "driver-secret-user", "driver-secret-password"]
  recordEvidence exception

recordEvidence :: SomeException -> IO ()
recordEvidence exception = do
  enabled <- lookupEnv "CH_TEST_LOG_METRICS"
  when (enabled == Just "1") $ hPutStrLn stderr $ takeWhile (/= '\n') (displayException exception)

withResponseServer :: ByteString -> Int -> ByteString -> (ClickhouseConnectionSettings ClientHTTP -> IO Int -> IO result) -> IO result
withResponseServer status advertisedLength payload = withLocalServer \connection ->
  SocketBS.sendAll connection $
    "HTTP/1.1 "
      <> status
      <> "\r\nContent-Length: "
      <> BSC.pack (show advertisedLength)
      <> "\r\nConnection: close\r\n\r\n"
      <> payload

withLocalServer :: (Socket -> IO ()) -> (ClickhouseConnectionSettings ClientHTTP -> IO Int -> IO result) -> IO result
withLocalServer responder =
  withLocalServerUsing
    2_000
    ( \connection -> do
        body <- receiveBody connection BS.empty
        responder connection
        pure (BS.length body)
    )

withLocalServerUsing :: Int -> (Socket -> IO Int) -> (ClickhouseConnectionSettings ClientHTTP -> IO Int -> IO result) -> IO result
withLocalServerUsing timeoutMS receiveRequest action = do
  receivedBytes <- newEmptyMVar
  withLocalServers timeoutMS [\client -> receiveRequest client >>= putMVar receivedBytes] \connection ->
    action connection (takeMVar receivedBytes)

withLocalServers :: Int -> [Socket -> IO ()] -> (ClickhouseConnectionSettings ClientHTTP -> IO result) -> IO result
withLocalServers timeoutMS responders action = bracket openListener close \listener -> do
  SockAddrInet serverPort _ <- getSocketName listener
  serverFinished <- newEmptyMVar
  server <-
    forkFinally
      (mapM_ (bracket (fst <$> accept listener) close) responders)
      (putMVar serverFinished)
  finally
    ( withConnection (fromIntegral serverPort) timeoutMS \connection -> do
        result <- action connection
        takeMVar serverFinished >>= either throwIO pure
        pure result
    )
    (killThread server)

withConnection :: Int -> Int -> (ClickhouseConnectionSettings ClientHTTP -> IO result) -> IO result
withConnection serverPort timeoutMS action = do
  let options =
        defaultHTTPSettings
          { clickhouseUrl = "http://127.0.0.1"
          , port = serverPort
          , responseTimeoutMS = timeoutMS
          , connectionTimeoutMS = 1_000
          }
  bracket (newHTTPTransport options) (CurlAgent.closeAgent . transportAgent) \transport -> do
    let connection =
          (defaultConnection transport)
            { username = "driver-secret-user"
            , password = "driver-secret-password"
            }
    action connection

openListener :: IO Socket
openListener = do
  listener <- socket AF_INET Stream defaultProtocol
  setSocketOption listener ReuseAddr 1
  bind listener (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
  listen listener 1
  pure listener

receiveBody :: Socket -> ByteString -> IO ByteString
receiveBody connection buffered = do
  (headerBlock, initialBody) <- receiveHeaders connection buffered
  let lengths = [value | line <- BSC.lines headerBlock, Just value <- [BSC.stripPrefix "Content-Length:" line]]
  required <- case lengths of
    [value] | Just (count, _) <- BSC.readInt (BSC.dropWhile (== ' ') value) -> pure count
    _ -> fail "missing Content-Length in local POST"
  readRemaining required initialBody
 where
  readRemaining required bytes
    | BS.length bytes >= required = pure (BS.take required bytes)
    | otherwise = receiveMore connection bytes >>= readRemaining required

receiveHeaders :: Socket -> ByteString -> IO (ByteString, ByteString)
receiveHeaders connection buffered =
  case BS.breakSubstring "\r\n\r\n" buffered of
    (headerBlock, suffix) | not (BS.null suffix) -> do
      pure (headerBlock, BS.drop 4 suffix)
    _ -> receiveMore connection buffered >>= receiveHeaders connection

receiveMore :: Socket -> ByteString -> IO ByteString
receiveMore connection bytes = do
  chunk <- SocketBS.recv connection 4_096
  unless (not $ BS.null chunk) $ fail "client closed before sending its request"
  pure (bytes <> chunk)
