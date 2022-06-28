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
import Network.HTTP.Conduit (RequestBody (..))
import Network.HTTP.Req (Scheme (..), http, https, renderUrl)
import Network.HTTP.Simple

mkClickHouseRequestSource :: MonadResource m => ClickhouseHTTPSettings -> ClickhouseConnectionSettings -> ByteString -> ConduitM i ByteString m ()
mkClickHouseRequestSource settings connection query = httpSource chRequest getResponseBody
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
        Https -> renderUrl $https host

chDefaultHeadersKV :: ClickhouseConnectionSettings -> RequestHeaders
chDefaultHeadersKV ClickhouseConnectionSettings{..} =
    [ ("X-ClickHouse-User", cs username)
    , ("X-ClickHouse-Key", cs password)
    , -- Used only when selecting data
      ("X-ClickHouse-Format", "TSVWithNamesAndTypes")
    , ("X-ClickHouse-Database", TE.encodeUtf8 dbScheme)
    ]
