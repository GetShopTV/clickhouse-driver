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
  , ClickhouseHTTPTransport (ClickhouseHTTPTransport, transportOptions, transportAgent)
  , transportConfig
  , withHTTPConfig
  , newManagedAgent
  , newHTTPTransport
  , newHTTPTransportWith
  , buildRequest
  ) where

import Control.Concurrent.MVar (MVar, newMVar, tryTakeMVar)
import Control.Exception (SomeAsyncException, SomeException, catch, evaluate, fromException, onException, throwIO)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Resource (MonadResource)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder (byteString, toLazyByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Conduit (ConduitT, yield)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import Data.UUID.V4 (nextRandom)
import Data.Vector qualified as Vector
import Database.Clickhouse.Client.HTTP.Diagnostics (httpStreamingWithMetrics)
import Database.Clickhouse.Client.HTTP.Types
import Database.Clickhouse.Client.Types
import Database.Clickhouse.Conversion.Binary.Encode (encodeRowsWithSettings)
import Database.Clickhouse.Conversion.Text.Escaped (effectiveRequestQueryParams)
import Database.Clickhouse.Conversion.Types (externalTypeContainsJSON)
import GHC.Clock (getMonotonicTimeNSec)
import HCurl.Agent (Agent)
import HCurl.Agent qualified as CurlAgent
import HCurl.Request qualified as Curl
import HCurl.Response (HttpParts (..), StreamingResponse (..))
import HCurl.Simple qualified as CurlSimple
import HCurl.Streaming qualified as CurlStream
import HCurl.Types qualified as CurlTypes
import Network.HTTP.Types (renderQuery)
import Numeric (showHex)
import System.IO.Unsafe (unsafePerformIO)
import UnliftIO (MonadUnliftIO)

-- | The HTTP transport implementation marker.
data ClientHTTP

-- | Transport settings of 'ClientHTTP': pure connection knobs plus the
-- user-owned hcurl agent every request is sent through.
data ClickhouseHTTPTransport
  = ClickhouseHTTPTransport
      { transportOptions :: !ClickhouseHTTPSettings
      , transportAgent :: !Agent
      }
  | ConfiguredHTTPTransport
      { transportOptions :: !ClickhouseHTTPSettings
      , transportAgent :: !Agent
      , configuredHTTPConfig :: !ClickhouseHTTPConfig
      }

-- | Read optional HTTP policy; legacy transports use 'defaultHTTPConfig'.
transportConfig :: ClickhouseHTTPTransport -> ClickhouseHTTPConfig
transportConfig ClickhouseHTTPTransport {} = defaultHTTPConfig
transportConfig ConfiguredHTTPTransport {configuredHTTPConfig = config} = config

-- | Configure an existing transport without replacing or taking ownership
-- of its agent. Record updates of connection options preserve this config.
withHTTPConfig :: ClickhouseHTTPConfig -> ClickhouseHTTPTransport -> ClickhouseHTTPTransport
withHTTPConfig config transport =
  ConfiguredHTTPTransport (transportOptions transport) (transportAgent transport) config

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

-- | Create a managed transport with application-owned HTTP policy and hooks.
newHTTPTransportWith :: ClickhouseHTTPSettings -> ClickhouseHTTPConfig -> IO ClickhouseHTTPTransport
newHTTPTransportWith options config = withHTTPConfig config <$> newHTTPTransport options

sendSourceHTTP ::
  (MonadResource m, MonadUnliftIO m) =>
  ClickhouseConnectionSettings ClientHTTP ->
  CHRequest ->
  ConduitT i ByteString m ()
sendSourceHTTP settings request = do
  let transport = connectionSettings settings
      config = transportConfig transport
      agent = transportAgent transport
  liftIO $ when (httpErrorBodyLimit config < 0) $ throwIO $ ClickhouseSettingsException "httpErrorBodyLimit must be nonnegative"
  httpRequest <- liftIO (buildHCurlRequest settings request)
  identifier <- liftIO nextRandom
  let bodyBytes = case Curl.body httpRequest of
        CurlTypes.Buffer bytes -> fromIntegral (BS.length bytes)
        CurlTypes.Empty -> 0
      onStarted = notifyHTTPEvent config (HTTPRequestStarted identifier bodyBytes)
      onFinished cancelled metrics =
        notifyHTTPEvent config $ HTTPRequestFinished identifier (outcomeFor cancelled metrics) metrics
      outcomeFor cancelled metrics
        | cancelled = HTTPTransferCancelled
        | transferCode metrics /= show CurlTypes.Ok = HTTPTransportFailed
        | maybe False (httpIsErrorStatus config) (responseStatus metrics) = HTTPResponseFailed
        | otherwise = HTTPTransferSucceeded
  (outcome, getMetrics) <-
    lift $ httpStreamingWithMetrics (httpStreamConfig config) onStarted onFinished agent httpRequest
  case outcome of
    Left code -> liftIO $ failWithTransportError code getMetrics
    Right StreamingResponse {info = responseHead@HttpParts {statusCode}, body = reader, completion} -> do
      isError <- liftIO $
        (do
          notifyHTTPEvent config (HTTPResponseReceived identifier statusCode)
          httpOnResponse config request responseHead
          evaluate (httpIsErrorStatus config statusCode)
        ) `onException` CurlStream.closeBody reader
      if isError
        then liftIO $ failWithServerError (httpErrorBodyLimit config) statusCode reader completion getMetrics
        else streamResponseBody reader completion getMetrics

notifyHTTPEvent :: ClickhouseHTTPConfig -> ClickhouseHTTPEvent -> IO ()
notifyHTTPEvent config event = httpOnEvent config event `catch` handleLoggerError
 where
  handleLoggerError :: SomeException -> IO ()
  handleLoggerError exception = case fromException exception :: Maybe SomeAsyncException of
    Just _ -> throwIO exception
    Nothing -> pure ()

-- | Drain the error body, wait for the transfer to finish and throw.
failWithServerError ::
  Int ->
  Int ->
  CurlStream.BodyReader ->
  IO (Either e a) ->
  IO ClickhouseTransferMetrics ->
  IO r
failWithServerError limit status reader completion getMetrics = do
  body <- drainBody limit reader getMetrics
  _ <- completion
  metrics <- getMetrics
  throwIO $
    ( ClickhouseServerException
        { serverStatus = status
        , serverMessage = body
        }
    )
      { serverMetrics = Just metrics
      }

failWithTransportError :: (Show code) => code -> IO ClickhouseTransferMetrics -> IO result
failWithTransportError code getMetrics = do
  metrics <- getMetrics
  throwIO $ (ClickhouseTransportException (show code)) {transportMetrics = Just metrics}

streamResponseBody ::
  (MonadIO m, Show e) =>
  CurlStream.BodyReader ->
  IO (Either e a) ->
  IO ClickhouseTransferMetrics ->
  ConduitT i ByteString m ()
streamResponseBody reader completion getMetrics = go
 where
  go = do
    chunk <- liftIO (CurlStream.readBody reader)
    case chunk of
      Left code -> liftIO $ failWithTransportError code getMetrics
      Right Nothing -> do
        final <- liftIO completion
        case final of
          Left code -> liftIO $ failWithTransportError code getMetrics
          Right _ -> pure ()
      Right (Just bytes) -> yield bytes >> go

drainBody :: Int -> CurlStream.BodyReader -> IO ClickhouseTransferMetrics -> IO ByteString
drainBody limit reader getMetrics = go limit []
 where
  go remaining acc = do
    chunk <- CurlStream.readBody reader
    case chunk of
      Left code -> failWithTransportError code getMetrics
      Right Nothing -> pure (BS.concat (reverse acc))
      Right (Just bytes) ->
        let retained = BS.take remaining bytes
         in go (remaining - BS.length retained) (if BS.null retained then acc else retained : acc)

buildHCurlRequest ::
  ClickhouseConnectionSettings ClientHTTP ->
  CHRequest ->
  IO Curl.Request
buildHCurlRequest conn request = do
  params <- either throwIO pure (effectiveRequestParams (settings conn) request)
  unless (settingEnabled "input_format_binary_read_json_as_string" params) $
    mapM_ validateExternal (requestExternals request)
  mapM_ (evaluate . BS.length) (requestData request)
  buildRequest conn (request {requestParams = params})
  where
    validateExternal table = mapM_ validateColumn (externalColumns table)
    validateColumn (_, typeName) = do
      hasJSON <- either (throwIO . ClickhouseSettingsException) pure (externalTypeContainsJSON typeName)
      if hasJSON
        then throwIO (ClickhouseSettingsException "JSON external tables require input_format_binary_read_json_as_string=1; enable it explicitly in settings")
        else pure ()

-- | Assemble the HTTP request for a 'CHRequest'.
--
-- A multipart body is used when the request has external tables or typed query
-- parameters.  Its field order is pinned: @query@ first, then one plain text
-- @param_<name>@ field per typed parameter, then the external-table parts.
-- An INSERT payload cannot travel in a multipart body, so typed parameters of
-- an INSERT request fall back to @param_<name>@ URL parameters (the SQL is
-- already sent as the URL @query@ parameter there).
--
-- The connection's 'extraHeaders' are appended to the auth headers of every
-- request.
--
-- Exported for tests, which pin the wire shape without a live server; the
-- transport itself is the only production caller.
buildRequest :: ClickhouseConnectionSettings ClientHTTP -> CHRequest -> IO Curl.Request
buildRequest conn request = do
  nativeRequest <- buildDefaultRequest conn request
  let config = transportConfig (connectionSettings conn)
  httpModifyRequest config request $
    nativeRequest {Curl.extraOptions = Curl.extraOptions nativeRequest <> httpExtraOptions config}

buildDefaultRequest :: ClickhouseConnectionSettings ClientHTTP -> CHRequest -> IO Curl.Request
buildDefaultRequest ClickhouseConnectionSettings {..} request@CHRequest {..} = do
  typedParams <- either throwIO pure (effectiveRequestQueryParams settings request)
  baseHeaders <- either throwIO pure ((authAndFormatHeaders <>) <$> effectiveExtraHeaders extraHeaders)
  case requestData of
    Just _ -> do
      unless (null requestExternals) $
        error "external tables cannot be combined with an INSERT payload"
      pure (makeRequest typedParams requestBody baseHeaders)
    Nothing
      | null typedParams && null requestExternals ->
          pure (makeRequest typedParams requestBody baseHeaders)
      | otherwise -> do
          boundary <- mkBoundary
          let multipartHeaders =
                baseHeaders
                  <> ["Content-Type: multipart/form-data; boundary=" <> boundary]
              payload = multipartBody boundary (multipartFields typedParams)
          _ <- evaluate (BS.length payload)
          pure (makeRequest typedParams (CurlTypes.Buffer payload) multipartHeaders)
  where
    httpOptions = transportOptions connectionSettings
    endpoint typedParams =
      clickhouseUrl httpOptions
        <> if port httpOptions == 0
          then mempty
          else ":" <> encodeUtf8Show (port httpOptions)
        <> renderQuery True (urlParams typedParams)
    -- 'requestParams' (settings and raw extras) always stay in the URL; typed
    -- parameters only join them when the body cannot carry a multipart form.
    urlParams typedParams = case requestData of
      Just _ ->
        ("query", Just requestSql)
          : ( map (\(name, value) -> (name, Just value)) requestParams
                <> map (\(name, value) -> (name, Just value)) typedParams
            )
      Nothing -> map (\(name, value) -> (name, Just value)) requestParams
    requestBody = case requestData of
      Just payload -> CurlTypes.Buffer payload
      Nothing
        | BS.null requestSql -> CurlTypes.Empty
        | otherwise -> CurlTypes.Buffer requestSql
    authAndFormatHeaders =
      [ "X-ClickHouse-User: " <> TE.encodeUtf8 username
      , "X-ClickHouse-Key: " <> TE.encodeUtf8 password
      , "X-ClickHouse-Database: " <> TE.encodeUtf8 database
      ]
        <> maybe [] (\format -> ["X-ClickHouse-Format: " <> format]) requestResponseFormat
    lowSpeed = lowSpeedLimit httpOptions
    makeRequest typedParams body headers =
      (Curl.defaultRequest (endpoint typedParams))
        { Curl.timeoutMS = responseTimeoutMS httpOptions
        , Curl.connectionTimeoutMS = connectionTimeoutMS httpOptions
        , Curl.lowSpeedLimit =
            Curl.LowSpeedLimit
              { Curl.lowSpeed = fst lowSpeed
              , Curl.timeout = snd lowSpeed
              }
        , Curl.body = body
        , Curl.method = CurlTypes.Post
        , Curl.headers = Curl.HeaderList headers
        }
    multipartFields typedParams =
      (MultipartField "query" Nothing Nothing requestSql : map typedParamField typedParams)
        <> concatMap externalFields requestExternals
    typedParamField (name, value) = MultipartField name Nothing Nothing value
    externalFields ExternalTable {..} =
      [ MultipartField (externalTableName <> "_format") Nothing Nothing "RowBinary"
      , MultipartField (externalTableName <> "_structure") Nothing Nothing (renderStructure externalColumns)
      , MultipartField
          externalTableName
          (Just externalTableName)
          (Just "application/octet-stream")
          (encodeRowsWithSettings requestParams (map Vector.fromList externalRows))
      ]

data MultipartField = MultipartField
  { fieldName :: !ByteString
  , fieldFileName :: !(Maybe ByteString)
  , fieldContentType :: !(Maybe ByteString)
  , fieldContent :: !ByteString
  }

renderStructure :: [(ByteString, ByteString)] -> ByteString
renderStructure =
  BS.intercalate ", " . map (\(name, typ) -> name <> " " <> typ)

mkBoundary :: IO ByteString
mkBoundary = do
  nanos <- getMonotonicTimeNSec
  pure ("----clickhouse-driver-" <> BS.pack (map (fromIntegral . fromEnum) (showHex nanos "")))

multipartBody :: ByteString -> [MultipartField] -> ByteString
multipartBody boundary fields =
  BSL.toStrict . toLazyByteString $
    foldMap (part boundary) fields <> closing boundary
  where
    part b MultipartField {..} =
      byteString ("--" <> b <> "\r\n")
        <> byteString ("Content-Disposition: form-data; name=\"" <> fieldName <> "\"")
        <> maybe mempty (\f -> byteString ("; filename=\"" <> f <> "\"")) fieldFileName
        <> byteString "\r\n"
        <> maybe mempty (\ct -> byteString ("Content-Type: " <> ct <> "\r\n")) fieldContentType
        <> byteString "\r\n"
        <> byteString fieldContent
        <> byteString "\r\n"
    closing b = byteString ("--" <> b <> "--\r\n")

encodeUtf8Show :: Show a => a -> ByteString
encodeUtf8Show = TE.encodeUtf8 . Text.pack . show
