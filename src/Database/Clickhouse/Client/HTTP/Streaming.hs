{-# LANGUAGE RecordWildCards #-}

module Database.Clickhouse.Client.HTTP.Streaming where

import Conduit
import Data.ByteString
import Data.Function
import Data.String
import Data.String.Conversions
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.Clickhouse.Client.HTTP.Types
import Database.Clickhouse.Types
import qualified Network.HTTP.Client as H
import Network.HTTP.Client.Conduit (bodyReaderSource)
import qualified Network.HTTP.Client.TLS as H
import Network.HTTP.Conduit (RequestBody (..))
import Network.HTTP.Req (Scheme (..), http, https, renderUrl)
import Network.HTTP.Simple

mkClickHouseRequestSource :: MonadIO m => ClickhouseHTTPSettings -> ClickhouseConnectionSettings -> ByteString -> Acquire (ConduitM i ByteString m ())
mkClickHouseRequestSource settings connection query = httpSourceA chRequest getResponseBody
 where
  ClickhouseHTTPSettings{..} = settings
  chRequest =
    fromString (T.unpack clickhouseUrl)
      & setRequestBody (RequestBodyBS query)
      & setRequestHeaders (chDefaultHeadersKV connection)
      & setRequestPort port
      & setRequestMethod "POST"
  clickhouseUrl = case scheme of
    Http -> renderUrl $ http host
    Https -> renderUrl $ https host

chDefaultHeadersKV :: ClickhouseConnectionSettings -> RequestHeaders
chDefaultHeadersKV ClickhouseConnectionSettings{..} =
  [ ("X-ClickHouse-User", cs username)
  , ("X-ClickHouse-Key", cs password)
  , -- Used only when selecting data
    ("X-ClickHouse-Format", "TSVWithNamesAndTypes")
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
