{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Clickhouse.Client.Execution (
  ClientExecution,
  ClickhouseExecution (..),
  ClickhouseRequestSource,
  executionFromSource,
  defaultExecution,
  withExecution,
) where

import Control.Monad.Trans.Resource (MonadResource)
import Data.ByteString (ByteString)
import Data.Conduit (ConduitT)
import Database.Clickhouse.Client.Types (
  CHRequest,
  ClickhouseClient (..),
  ClickhouseConnectionSettings (..),
 )
import UnliftIO (MonadUnliftIO)

data ClientExecution

type ClickhouseRequestSource =
  forall input monad.
  (MonadResource monad, MonadUnliftIO monad) =>
  ClickhouseConnectionSettings ClientExecution ->
  CHRequest ->
  ConduitT input ByteString monad ()

data ClickhouseExecution = ClickhouseExecution
  { executeSource :: ClickhouseRequestSource
  , executeRequest ::
      forall result.
      ClickhouseConnectionSettings ClientExecution ->
      CHRequest ->
      IO result ->
      IO result
  }

instance ClickhouseClient ClientExecution where
  type ClickhouseClientSettings ClientExecution = ClickhouseExecution

  sendSource connection request =
    executeSource (connectionSettings connection) connection request

  runClientRequest connection request action =
    executeRequest (connectionSettings connection) connection request action

executionFromSource :: ClickhouseRequestSource -> ClickhouseExecution
executionFromSource source =
  ClickhouseExecution
    { executeSource = source
    , executeRequest = \_ _ action -> action
    }

defaultExecution :: ClickhouseClient client => ClickhouseConnectionSettings client -> ClickhouseExecution
defaultExecution connection =
  ClickhouseExecution
    { executeSource = \current request ->
        sendSource (replaceTransport (connectionSettings connection) current) request
    , executeRequest = \current request action ->
        runClientRequest (replaceTransport (connectionSettings connection) current) request action
    }

withExecution :: ClickhouseExecution -> ClickhouseConnectionSettings client -> ClickhouseConnectionSettings ClientExecution
withExecution = replaceTransport

replaceTransport :: ClickhouseClientSettings target -> ClickhouseConnectionSettings source -> ClickhouseConnectionSettings target
replaceTransport transport ClickhouseConnectionSettings{..} =
  ClickhouseConnectionSettings
    { username = username
    , password = password
    , database = database
    , settings = settings
    , extraHeaders = extraHeaders
    , connectionSettings = transport
    }
