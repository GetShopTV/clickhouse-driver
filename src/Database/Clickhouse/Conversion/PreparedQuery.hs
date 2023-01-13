module Database.Clickhouse.Conversion.PreparedQuery where

import Conduit
import Control.Exception.Base (throw)
import Data.Binary.Put (Put, execPut, putByteString, putCharUtf8, putLazyByteString)
import Data.ByteString
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Lazy.Char8 qualified as LC
import Data.Csv qualified as CSV
import Data.Foldable
import Data.Kind
import Database.Clickhouse.Client.Types
import Database.Clickhouse.Conversion.Types

-- | Renderer of prepared query with question marks (?) placeholders in place of values.
data PreparedRenderer :: Type

newtype PreparedQuery = PreparedQuery {unPreparedQuery :: BSL.ByteString}

instance QueryRenderer PreparedRenderer where
  type RenderQueryType PreparedRenderer = PreparedQuery
  renderRow = renderPrepared
  renderRows query rows = renderRow query $ do
    row <- toList rows
    toList row

renderPrepared :: Foldable f => PreparedQuery -> f ClickhouseType -> Query
renderPrepared (PreparedQuery qry) paramsFoldable =
  Query $ (yield . execPut $ merge fragments params) .| builderToByteString
 where
  params = toList paramsFoldable
  fragments = LC.split '?' qry
  merge [x] [] = putLazyByteString x
  merge (x : xs) (y : ys) = putLazyByteString x >> renderPreparedClickhouseType y >> merge xs ys
  merge _ _ = throw WrongParamsCount

renderPreparedClickhouseType :: ClickhouseType -> Put
renderPreparedClickhouseType ct@(ClickString _) = quoted $ CSV.toField ct
renderPreparedClickhouseType ct@(ClickDateTime _) = quoted $ CSV.toField ct
renderPreparedClickhouseType ct@(ClickDate _) = quoted $ CSV.toField ct
renderPreparedClickhouseType ct@(ClickUUID _) = quoted $ CSV.toField ct
renderPreparedClickhouseType ct@(ClickIPv4 _) = quoted $ CSV.toField ct
renderPreparedClickhouseType ct@(ClickIPv6 _) = quoted $ CSV.toField ct
renderPreparedClickhouseType (ClickNullable Nothing) = putByteString "null"
renderPreparedClickhouseType ct = putByteString $ CSV.toField ct

quoted :: ByteString -> Put
quoted bs = putCharUtf8 '\'' >> putByteString bs >> putCharUtf8 '\''