{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Database.Clickhouse.Client.HTTP.Client where

import Control.Monad.Catch (MonadThrow)
import Data.ByteString.Char8 (ByteString)
import Data.Kind (Type)
import Data.String.Conversions (cs)
import qualified Data.Text.Encoding as TE
import Database.Clickhouse.Client.HTTP.Types
  ( ClickhouseHTTPSettings (..),
  )
import Database.Clickhouse.Types
  ( ClickhouseClient (..),
    ClickhouseConnectionSettings (..),
    runQuery,
  )
import Network.HTTP.Req as R
  ( MonadHttp,
    Option,
    POST (POST),
    ReqBodyBs (ReqBodyBs),
    Scheme (Http, Https),
    bsResponse,
    defaultHttpConfig,
    header,
    http,
    https,
    req,
    responseBody,
    runReq,
  )
import qualified Network.HTTP.Req as R

type ClientHTTP :: Type
data ClientHTTP

instance ClickhouseClient ClientHTTP where
  type ClickhouseClientSettings ClientHTTP = ClickhouseHTTPSettings
  send settings env query = do
    let queryBS = runQuery query
    runReq defaultHttpConfig $ mkClickHouseRequest settings env queryBS

mkClickHouseRequest :: (MonadHttp m, MonadThrow m) => ClickhouseHTTPSettings -> ClickhouseConnectionSettings -> ByteString -> m ByteString
mkClickHouseRequest settings connection query = do
  let body = ReqBodyBs query
  case scheme of
    Https -> do
      response <-
        req POST (https host) body bsResponse (R.port port <> chDefaultHeaders connection)
      pure (responseBody response)
    Http -> do
      response <-
        req POST (http host) body bsResponse (R.port port <> chDefaultHeaders connection)
      pure (responseBody response)
  where
    ClickhouseHTTPSettings {..} = settings

chDefaultHeaders :: ClickhouseConnectionSettings -> Option scheme
chDefaultHeaders ClickhouseConnectionSettings {..} =
  mconcat
    [ header "X-ClickHouse-User" (cs username),
      header "X-ClickHouse-Key" (cs password),
      -- Used only when selecting data
      header "X-ClickHouse-Format" "TSVWithNamesAndTypes",
      header "X-ClickHouse-Database" (TE.encodeUtf8 dbScheme)
    ]
