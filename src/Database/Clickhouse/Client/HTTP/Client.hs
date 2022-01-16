{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Database.Clickhouse.Client.HTTP.Client where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Kind
import Data.List.NonEmpty (NonEmpty (..), fromList, toList)
import Data.String.Conversions (cs)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.UUID as UUID
import Data.Vector (Vector)
import qualified Data.Vector as V
import Database.Clickhouse.Client.HTTP.Types
import Database.Clickhouse.Types
import Network.HTTP.Req as R

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
  -- FIXME: Remove deubg reporting
  liftIO $ BS.putStrLn "\n"
  liftIO $ BS.putStrLn query
  liftIO $ BS.putStrLn "\n"
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
    ch@ClickhouseHTTPSettings {..} = settings

chDefaultHeaders :: ClickhouseConnectionSettings -> Option scheme
chDefaultHeaders connection@ClickhouseConnectionSettings {..} =
  mconcat
    [ header "X-ClickHouse-User" (cs username),
      header "X-ClickHouse-Key" (cs password),
      -- Used only when selecting data
      header "X-ClickHouse-Format" "TSVWithNamesAndTypes",
      header "X-ClickHouse-Database" (TE.encodeUtf8 dbScheme)
    ]