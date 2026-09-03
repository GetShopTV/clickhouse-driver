{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Pure connection knobs of the hcurl (libcurl) based transport.

The agent itself is not part of these settings: it is created explicitly and
owned by the caller (see 'Database.Clickhouse.Client.HTTP.Client.newManagedAgent').
-}
module Database.Clickhouse.Client.HTTP.Types
  ( ClickhouseHTTPSettings (..)
  , defaultHTTPSettings
  ) where

import Data.ByteString (ByteString)

-- | Connection knobs of the HTTP transport.
--
-- 'clickhouseUrl' is the scheme + host of the server (e.g.
-- @http:\/\/localhost@ or @https:\/\/ch.example.com@) and must not contain a
-- port or a trailing slash; the port is appended separately (use @0@ to omit
-- it if the URL already carries one).
data ClickhouseHTTPSettings = ClickhouseHTTPSettings
  { clickhouseUrl :: !ByteString
  , port :: !Int
  , -- | Total request timeout in milliseconds; @0@ disables it.
    responseTimeoutMS :: !Int
  , -- | Connect timeout in milliseconds.
    connectionTimeoutMS :: !Int
  , -- | Abort when the transfer falls below @fst@ bytes/s for @snd@ seconds.
    -- @(0, 0)@ disables the low-speed limit.
    lowSpeedLimit :: !(Int, Int)
  }
  deriving stock (Show, Eq)

-- | Defaults: @http:\/\/localhost:8123@, no total timeout, 10 second connect
-- timeout and no low-speed limit.
defaultHTTPSettings :: ClickhouseHTTPSettings
defaultHTTPSettings =
  ClickhouseHTTPSettings
    { clickhouseUrl = "http://localhost"
    , port = 8123
    , responseTimeoutMS = 0
    , connectionTimeoutMS = 10000
    , lowSpeedLimit = (0, 0)
    }
