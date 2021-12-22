{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Database.Clickhouse.Client where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Coerce
import Data.DoubleWord
import Data.Either
import Data.List.NonEmpty (NonEmpty (..), fromList, toList)
import Data.Proxy
import Data.String.Conversions (cs)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.UUID as UUID
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word
import Database.Clickhouse.Generic
import Database.Clickhouse.Render
import Database.Clickhouse.Types
import GHC.TypeLits
import Network.HTTP.Req as R

mkClickHouseRequest :: (MonadHttp m, MonadThrow m) => ClickhouseEnv -> ByteString -> m ByteString
mkClickHouseRequest connection query = do
  let body = ReqBodyBs query
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
    ch@ClickhouseSettings {..} = settings connection

chDefaultHeaders :: ClickhouseEnv -> Option scheme
chDefaultHeaders connection@ClickhouseEnv {..} =
  mconcat
    [ header "X-ClickHouse-User" (cs username),
      header "X-ClickHouse-Key" (cs password),
      header "X-ClickHouse-Format" "TSVWithNamesAndTypes",
      header "X-ClickHouse-Database" (TE.encodeUtf8 dbScheme)
    ]
  where
    ClickhouseSettings {..} = settings

execute :: (MonadHttp m, MonadThrow m) => ClickhouseEnv -> Query -> [ClickhouseType] -> m ByteString
execute connection query params = mkClickHouseRequest connection (runQuery $ renderParams query params)

-- insertMany

-- | Insert raw fields list
insertMultiple ::
  (MonadHttp m, MonadThrow m) =>
  ClickhouseEnv ->
  ByteString ->
  NonEmpty [Field] ->
  m ByteString
insertMultiple connection tableName records = do
  let (errors, valids) = partitionEithers $ toList (traverse (\(tname, edata) -> (tname,) <$> edata) <$> records)
  errorReport errors
  mkClickHouseRequest connection (query tableName (fromList valids))
  where
    errorReport [] = pure ()
    errorReport errors = error . cs $ "Cannot parse clickhouse-representation" <> T.intercalate "\n" errors

-- | Insert structs with ClickRep instance
insertRecords ::
  (MonadHttp m, MonadThrow m, ClickRep a) =>
  ClickhouseEnv ->
  ByteString ->
  NonEmpty a ->
  m ByteString
insertRecords connection tableName recordsObjects =
  insertMultiple connection tableName (toClickRep <$> recordsObjects)

-- Render to INSERT
query :: ByteString -> NonEmpty [(ByteString, ClickhouseType)] -> ByteString
query tableName records =
  "INSERT INTO "
    <> tableName
    <> " "
    <> toClickList (columns records)
    <> " FORMAT Values "
    <> BS.intercalate ", " (toList $ recordsList records)
