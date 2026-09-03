{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}

{- |
ClickHouse transport implemented on top of hcurl (libcurl multi interface).

The transport never creates an agent implicitly.  The caller owns the hcurl
agent and passes it in via 'ClickhouseHTTPTransport':

  * 'newManagedAgent' spawns the driver's default agent topology (hcurl's
    managed agent with its default policy) and performs the one-off libcurl
    global initialisation;
  * an externally created agent (hcurl's 'spawnAgent', 'spawnThreadedAgent',
    'spawnManagedAgent') can be used instead — in that case the caller is
    responsible for calling 'HCurl.Simple.initCurl' once first.

Requests are plain HTTP POSTs; responses are consumed as a streaming
'BodyReader' and yielded chunk by chunk by the conduit.

Error reporting:

  * curl-level failures throw 'ClickhouseTransportException';
  * HTTP statuses >= 400 drain the (typically small) error body and throw
    'ClickhouseServerException' with the server text;
  * failures that only surface at end-of-stream (truncated bodies) are thrown
    when the returned conduit is fully drained.
-}
module Database.Clickhouse.Client.HTTP.Client
  ( ClientHTTP
  , ClickhouseHTTPTransport (..)
  , newManagedAgent
  , newHTTPTransport
  ) where

import Control.Concurrent.MVar (MVar, newMVar, tryTakeMVar)
import Control.Exception (throwIO)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Resource (MonadResource)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Conduit (ConduitT, yield)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import Database.Clickhouse.Client.HTTP.Types (ClickhouseHTTPSettings (..))
import Database.Clickhouse.Client.Types
import HCurl.Agent (Agent)
import HCurl.Agent qualified as CurlAgent
import HCurl.Request qualified as Curl
import HCurl.Response (HttpParts (..), StreamingResponse (..))
import HCurl.Simple qualified as CurlSimple
import HCurl.Streaming qualified as CurlStream
import HCurl.Types qualified as CurlTypes
import Network.HTTP.Types (renderQuery)
import System.IO.Unsafe (unsafePerformIO)
import UnliftIO (MonadUnliftIO)

-- | The HTTP transport implementation marker.
data ClientHTTP

-- | Transport settings of 'ClientHTTP': pure connection knobs plus the
-- user-owned hcurl agent every request is sent through.
data ClickhouseHTTPTransport = ClickhouseHTTPTransport
  { transportOptions :: !ClickhouseHTTPSettings
  , transportAgent :: !Agent
  }

instance ClickhouseClient ClientHTTP where
  type ClickhouseClientSettings ClientHTTP = ClickhouseHTTPTransport
  sendSource = sendSourceHTTP

-- | One-shot libcurl global initialisation, shared by every agent created
-- through 'newManagedAgent'.
{-# NOINLINE curlInitGate #-}
curlInitGate :: MVar ()
curlInitGate = unsafePerformIO (newMVar ())

initCurlOnce :: IO ()
initCurlOnce = do
  taken <- tryTakeMVar curlInitGate
  case taken of
    Just () -> CurlSimple.initCurl
    Nothing -> pure ()

-- | Create the driver's default agent: hcurl's managed agent with its
-- default policy (see 'HCurl.Agent.defaultManagedPolicy').
newManagedAgent :: IO Agent
newManagedAgent = do
  initCurlOnce
  policy <- CurlAgent.defaultManagedPolicy
  CurlAgent.spawnManagedAgent policy CurlTypes.defaultConfig

-- | Wrap connection knobs into transport settings using a freshly created
-- default managed agent.
newHTTPTransport :: ClickhouseHTTPSettings -> IO ClickhouseHTTPTransport
newHTTPTransport options = do
  agent <- newManagedAgent
  pure $ ClickhouseHTTPTransport {transportOptions = options, transportAgent = agent}

sendSourceHTTP ::
  (MonadResource m, MonadUnliftIO m) =>
  ClickhouseConnectionSettings ClientHTTP ->
  CHRequest ->
  ConduitT i ByteString m ()
sendSourceHTTP settings request = do
  let ClickhouseHTTPTransport {transportOptions = _, transportAgent = agent} =
        connectionSettings settings
  outcome <-
    lift $ CurlStream.httpStreaming agent (buildHCurlRequest settings request)
  case outcome of
    Left code -> liftIO $ throwIO $ ClickhouseTransportException (show code)
    Right StreamingResponse {info = HttpParts {statusCode}, body = reader, completion} ->
      if statusCode >= 400
        then liftIO $ failWithServerError statusCode reader completion
        else streamResponseBody reader completion

-- | Drain the error body, wait for the transfer to finish and throw.
failWithServerError ::
  Int ->
  CurlStream.BodyReader ->
  IO (Either e a) ->
  IO r
failWithServerError status reader completion = do
  body <- drainBody reader
  _ <- completion
  throwIO $
    ClickhouseServerException
      { serverStatus = status
      , serverMessage = BS.take 4096 body
      }

streamResponseBody ::
  (MonadIO m, Show e) =>
  CurlStream.BodyReader ->
  IO (Either e a) ->
  ConduitT i ByteString m ()
streamResponseBody reader completion = go
  where
    go = do
      chunk <- liftIO (CurlStream.readBody reader)
      case chunk of
        Left code -> liftIO $ throwIO $ ClickhouseTransportException (show code)
        Right Nothing -> do
          final <- liftIO completion
          case final of
            Left code -> liftIO $ throwIO $ ClickhouseTransportException (show code)
            Right _ -> pure ()
        Right (Just bytes) -> yield bytes >> go

drainBody :: CurlStream.BodyReader -> IO ByteString
drainBody reader = go []
  where
    go acc = do
      chunk <- CurlStream.readBody reader
      case chunk of
        Left code -> throwIO $ ClickhouseTransportException (show code)
        Right Nothing -> pure (BS.concat (reverse acc))
        Right (Just bytes) -> go (bytes : acc)

buildHCurlRequest ::
  ClickhouseConnectionSettings ClientHTTP ->
  CHRequest ->
  Curl.Request
buildHCurlRequest ClickhouseConnectionSettings {..} CHRequest {..} =
  Curl.Request
    { Curl.host = endpoint
    , Curl.timeoutMS = responseTimeoutMS httpOptions
    , Curl.connectionTimeoutMS = connectionTimeoutMS httpOptions
    , Curl.lowSpeedLimit =
        Curl.LowSpeedLimit
          { Curl.lowSpeed = fst lowSpeed
          , Curl.timeout = snd lowSpeed
          }
    , Curl.body = requestBody
    , Curl.method = CurlTypes.Post
    , Curl.headers = Curl.HeaderList requestHeaders
    , Curl.extraOptions = []
    }
  where
    httpOptions = transportOptions connectionSettings
    endpoint =
      clickhouseUrl httpOptions
        <> if port httpOptions == 0
          then mempty
          else ":" <> encodeUtf8Show (port httpOptions)
        <> renderQuery
          True
          ( case requestData of
              Just _ -> ("query", Just requestSql) : map (\(k, v) -> (k, Just v)) requestParams
              Nothing -> map (\(k, v) -> (k, Just v)) requestParams
          )
    requestBody = case requestData of
      Just payload -> CurlTypes.Buffer payload
      Nothing
        | BS.null requestSql -> CurlTypes.Empty
        | otherwise -> CurlTypes.Buffer requestSql
    requestHeaders =
      [ "X-ClickHouse-User: " <> TE.encodeUtf8 username
      , "X-ClickHouse-Key: " <> TE.encodeUtf8 password
      , "X-ClickHouse-Database: " <> TE.encodeUtf8 database
      ]
        <> maybe [] (\format -> ["X-ClickHouse-Format: " <> format]) requestResponseFormat
    lowSpeed = lowSpeedLimit httpOptions

encodeUtf8Show :: Show a => a -> ByteString
encodeUtf8Show = TE.encodeUtf8 . Text.pack . show
