{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Database.Clickhouse.Client.HTTP.Client where

import Data.Kind (Type)
import Data.Text.Encoding qualified as TE
import Database.Clickhouse.Client.Types

import Conduit
import Data.ByteString
import Data.Function
import Data.String
import Data.String.Conversions
import Database.Clickhouse.Client.HTTP.Types
import Network.HTTP.Client qualified as H
import Network.HTTP.Client.Conduit hiding (httpSource)
import Network.HTTP.Client.TLS qualified as H
import Network.HTTP.Simple hiding (Query)

type ClientHTTP :: Type
data ClientHTTP

instance ClickhouseClient ClientHTTP where
    type ClickhouseClientSettings ClientHTTP = ClickhouseHTTPSettings
    sendSource settings query = do
        mkClickHouseRequestSource settings query

instance ClickhouseClientAcquire ClientHTTP where
    sendSourceAcquire settings query = do
        mkClickHouseRequestSourceAсquire settings query

mkClickHouseRequestSource ::
    (MonadResource m) =>
    ClickhouseConnectionSettings ClientHTTP ->
    Query ->
    ConduitM i ByteString m ()
mkClickHouseRequestSource settings (Query query) = httpSource chRequest getResponseBody
  where
    ClickhouseHTTPSettings{..} = connectionSettings settings
    chRequest =
        fromString clickhouseUrl
            & setRequestBody (requestBodySourceChunked query)
            & setRequestHeaders (chDefaultHeadersKV settings)
            & setRequestPort port
            & setRequestMethod "POST"
            & setRequestResponseTimeout responseTimeout

mkClickHouseRequestSourceAсquire :: (MonadIO m) => ClickhouseConnectionSettings ClientHTTP -> Query -> Acquire (ConduitM i ByteString m ())
mkClickHouseRequestSourceAсquire settings (Query query) = httpSourceA chRequest getResponseBody
  where
    ClickhouseHTTPSettings{..} = connectionSettings settings
    chRequest =
        fromString clickhouseUrl
            & setRequestBody (requestBodySourceChunked query)
            & setRequestHeaders (chDefaultHeadersKV settings)
            & setRequestPort port
            & setRequestMethod "POST"
            & setRequestResponseTimeout responseTimeout

chDefaultHeadersKV :: ClickhouseConnectionSettings ClientHTTP -> RequestHeaders
chDefaultHeadersKV ClickhouseConnectionSettings{..} =
    [ ("X-ClickHouse-User", cs username)
    , ("X-ClickHouse-Key", cs password)
    , -- Used only when selecting data
      ("X-ClickHouse-Format", "CSVWithNamesAndTypes")
    , ("X-ClickHouse-Database", TE.encodeUtf8 dbScheme)
    ]

httpSourceA ::
    (MonadIO n) =>
    H.Request ->
    ( H.Response (ConduitM i ByteString n ()) ->
      ConduitM i o m r
    ) ->
    Acquire (ConduitM i o m r)
httpSourceA req withRes = do
    man <- liftIO H.getGlobalManager
    let ack = mkAcquire (H.responseOpen req man) H.responseClose
    withRes . fmap bodyReaderSource <$> ack
