{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Connection knobs, HTTP policy and application hooks for the hcurl transport.

The agent itself is not part of these settings: it is created explicitly and
owned by the caller (see 'Database.Clickhouse.Client.HTTP.Client.newManagedAgent').
-}
module Database.Clickhouse.Client.HTTP.Types (
  ClickhouseHTTPSettings (..),
  defaultHTTPSettings,
  ClickhouseHTTPConfig (..),
  defaultHTTPConfig,
  ClickhouseHTTPEvent (..),
  ClickhouseHTTPOutcome (..),
  SomeOption (..),
  StreamConfig (..),
) where

import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.UUID (UUID)
import Database.Clickhouse.Client.Types (CHRequest, ClickhouseTransferMetrics)
import HCurl.Options (SomeOption (..))
import HCurl.Request (Request)
import HCurl.Response (HttpParts)
import HCurl.Streaming (StreamConfig (..), defaultStreamConfig)

{- | Connection knobs of the HTTP transport.

'clickhouseUrl' is the scheme + host of the server (e.g.
@http:\/\/localhost@ or @https:\/\/ch.example.com@) and must not contain a
port or a trailing slash; the port is appended separately (use @0@ to omit
it if the URL already carries one).
-}
data ClickhouseHTTPSettings = ClickhouseHTTPSettings
  { clickhouseUrl :: !ByteString
  , port :: !Int
  , responseTimeoutMS :: !Int
  -- ^ Total request timeout in milliseconds; @0@ disables it.
  , connectionTimeoutMS :: !Int
  -- ^ Connect timeout in milliseconds.
  , lowSpeedLimit :: !(Int, Int)
  {- ^ Abort when the transfer falls below @fst@ bytes/s for @snd@ seconds.
  @(0, 0)@ disables the low-speed limit.
  -}
  }
  deriving stock (Show, Eq)

{- | Defaults: @http:\/\/localhost:8123@, no total timeout, 10 second connect
timeout and no low-speed limit.
-}
defaultHTTPSettings :: ClickhouseHTTPSettings
defaultHTTPSettings =
  ClickhouseHTTPSettings
    { clickhouseUrl = "http://localhost"
    , port = 8123
    , responseTimeoutMS = 0
    , connectionTimeoutMS = 10000
    , lowSpeedLimit = (0, 0)
    }

{- | Optional HTTP policy and application-owned hooks. Connection credentials
and ClickHouse settings remain in @ClickhouseConnectionSettings@.
-}
data ClickhouseHTTPConfig = ClickhouseHTTPConfig
  { httpExtraOptions :: ![SomeOption]
  -- ^ Applied after the driver's native defaults, before 'httpModifyRequest'.
  , httpStreamConfig :: !StreamConfig
  -- ^ Bounded response buffering; the capacity must be positive.
  , httpErrorBodyLimit :: !Int
  {- ^ Maximum error-body bytes retained in a server exception. The rest is
  drained without retaining it. Must be nonnegative; @0@ retains no text.
  -}
  , httpModifyRequest :: CHRequest -> Request -> IO Request
  {- ^ Transform the fully built native request before submission, including
  its multipart boundaries. Send functions resolve effective ClickHouse
  settings first. Exceptions propagate without submitting the request.
  -}
  , httpOnResponse :: CHRequest -> HttpParts -> IO ()
  {- ^ Observe the response head before reading its body. This hook may
  inspect headers or reject a response by throwing an exception.
  -}
  , httpIsErrorStatus :: Int -> Bool
  -- ^ Decide which final HTTP statuses are server errors.
  , httpOnEvent :: ClickhouseHTTPEvent -> IO ()
  {- ^ Receive ordered events on the request/resource-cleanup thread, never
  on the curl reactor. Synchronous logger exceptions are ignored;
  asynchronous exceptions propagate. A blocking callback delays its caller.
  -}
  }

{- | Preserve the original transport behaviour: native curl defaults,
sixteen buffered response chunks, HTTP statuses >= 400 rejected, 4096
retained error bytes, and no-op hooks.
-}
defaultHTTPConfig :: ClickhouseHTTPConfig
defaultHTTPConfig =
  ClickhouseHTTPConfig
    { httpExtraOptions = []
    , httpStreamConfig = defaultStreamConfig
    , httpErrorBodyLimit = 4096
    , httpModifyRequest = \_ request -> pure request
    , httpOnResponse = \_ _ -> pure ()
    , httpIsErrorStatus = (>= 400)
    , httpOnEvent = \_ -> pure ()
    }

-- | Transport outcome, not the outcome of decoding or consuming query rows.
data ClickhouseHTTPOutcome
  = HTTPTransferSucceeded
  | HTTPTransportFailed
  | HTTPResponseFailed
  | HTTPTransferCancelled
  deriving stock (Show, Eq)

{- | Correlated diagnostic events. No SQL, parameters, credentials, URLs,
headers or response bodies are included. A submitted transfer produces
exactly one terminal event, including when its resource scope is cancelled.
-}
data ClickhouseHTTPEvent
  = HTTPRequestStarted
      { eventRequestId :: !UUID
      , eventRequestBodyBytes :: !Int64
      }
  | HTTPResponseReceived
      { eventRequestId :: !UUID
      , eventResponseStatus :: !Int
      }
  | HTTPRequestFinished
      { eventRequestId :: !UUID
      , eventOutcome :: !ClickhouseHTTPOutcome
      , eventMetrics :: !ClickhouseTransferMetrics
      }
  deriving stock (Show, Eq)
