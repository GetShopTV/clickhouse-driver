{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}

{- |
ClickHouse transport implemented on top of hcurl (libcurl multi interface).

The client keeps a single process-wide curl agent, created lazily on first
use.  Requests are plain HTTP POSTs; responses are consumed as a streaming
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
  ) where

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

instance ClickhouseClient ClientHTTP where
  type ClickhouseClientSettings ClientHTTP = ClickhouseHTTPSettings
  sendSource = sendSourceHTTP

-- | A lazily initialised, process-wide curl agent.
{-# NOINLINE processAgent #-}
processAgent :: CurlAgent.Agent
processAgent =
  unsafePerformIO $ do
    CurlSimple.initCurl
    CurlAgent.spawnAgent CurlTypes.defaultConfig

sendSourceHTTP ::
  (MonadResource m, MonadUnliftIO m) =>
  ClickhouseConnectionSettings ClientHTTP ->
  CHRequest ->
  ConduitT i ByteString m ()
sendSourceHTTP settings request = do
  outcome <-
    lift $ CurlStream.httpStreaming processAgent (buildHCurlRequest settings request)
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
    , Curl.timeoutMS = responseTimeoutMS httpSettings
    , Curl.connectionTimeoutMS = connectionTimeoutMS httpSettings
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
    httpSettings = connectionSettings
    endpoint =
      clickhouseUrl httpSettings
        <> if port httpSettings == 0
          then mempty
          else ":" <> (encodeUtf8Show (port httpSettings))
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
    lowSpeed = lowSpeedLimit httpSettings

encodeUtf8Show :: Show a => a -> ByteString
encodeUtf8Show = TE.encodeUtf8 . Text.pack . show
