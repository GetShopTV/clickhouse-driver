module Database.Clickhouse.Conversion.Values.Renderer where

import Control.Exception.Base (throw)
import Data.Binary.Put (putByteString, putLazyByteString, runPut)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Foldable
import Data.Kind
import Database.Clickhouse.Conversion.Types
import Database.Clickhouse.Types
import Database.Clickhouse.Conversion.FromClickhouse

-- | Renderer of prepared query with question marks (?) placeholders in place of values.
data ValuesRenderer :: Type

newtype PreparedQuery = PreparedQuery {unPreparedQuery :: BSL.ByteString}

instance QueryRenderer ValuesRenderer where
  type RenderQueryType ValuesRenderer = PreparedQuery
  renderRow = renderPrepared
  renderRows query rows = renderRow query $ do
    row <- toList rows
    toList row

renderPrepared :: Foldable f => PreparedQuery -> f ClickhouseType -> Query
renderPrepared (PreparedQuery qry) paramsFoldable =
  let params = toList paramsFoldable
      fragments = LC.split '?' qry
   in Query . runPut $ merge fragments params
  where
    merge [x] [] = putLazyByteString x
    merge (x : xs) (y : ys) = putLazyByteString x >> putByteString (fromClickhouseType y) >> merge xs ys
    merge _ _ = throw WrongParamsCount
