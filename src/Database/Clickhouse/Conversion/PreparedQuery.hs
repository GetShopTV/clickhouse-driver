module Database.Clickhouse.Conversion.PreparedQuery where

import Conduit
import Control.Exception.Base (throw)
import Data.Binary.Put (putByteString, putLazyByteString, runPut)
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Lazy.Char8 qualified as LC
import Data.Foldable
import Data.Kind
import Database.Clickhouse.Conversion.FromClickhouse
import Database.Clickhouse.Conversion.Types
import Database.Clickhouse.Client.Types

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
  let params = toList paramsFoldable
      fragments = LC.split '?' qry
   in Query . sourceLazy . runPut $ merge fragments params
 where
  merge [x] [] = putLazyByteString x
  merge (x : xs) (y : ys) = putLazyByteString x >> putByteString (fromClickhouseType y) >> merge xs ys
  merge _ _ = throw WrongParamsCount
