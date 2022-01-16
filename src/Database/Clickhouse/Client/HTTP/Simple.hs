module Database.Clickhouse.Client.HTTP.Simple where

import Control.Monad.Catch
import Data.ByteString
import qualified Data.ByteString as BS
import Data.List.NonEmpty
import Database.Clickhouse.Client.HTTP.Client
import Database.Clickhouse.Client.HTTP.Types
import Database.Clickhouse.Conversion.Bytestring.To
import Database.Clickhouse.Conversion.Generic.ClickRep
import Database.Clickhouse.Types
import Network.HTTP.Req

-- insertMany

-- | Insert raw fields list
insertMultiple ::
  (MonadHttp m, MonadThrow m) =>
  ClickhouseHTTPSettings ->
  ClickhouseConnectionSettings ->
  ByteString ->
  NonEmpty [Field] ->
  m ByteString
insertMultiple settings connection tableName records = do
  let typeValuePairs = toList records
  mkClickHouseRequest settings connection (query tableName (fromList typeValuePairs))

-- | Insert structs with ClickRep instance
insertRecords ::
  (MonadHttp m, MonadThrow m, ClickRep a) =>
  ClickhouseHTTPSettings ->
  ClickhouseConnectionSettings ->
  ByteString ->
  NonEmpty a ->
  m ByteString
insertRecords settings connection tableName recordsObjects =
  insertMultiple settings connection tableName (toClickRep <$> recordsObjects)

-- Render to INSERT
query :: ByteString -> NonEmpty [(ByteString, ClickhouseType)] -> ByteString
query tableName records =
  "INSERT INTO "
    <> tableName
    <> " "
    <> toClickList (columns records)
    <> " FORMAT Values "
    <> BS.intercalate ", " (toList $ recordsList records)
