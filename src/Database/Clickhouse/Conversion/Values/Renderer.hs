module Database.Clickhouse.Conversion.Values.Renderer where

import Control.Exception (Exception)
import Control.Exception.Base (throw)
import Data.Binary.Put (putByteString, putLazyByteString, runPut)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Data (Typeable)
import Data.Foldable
import Data.IP
import Data.Int
import Data.Kind
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.String.Conversions (cs)
import Data.Time (defaultTimeLocale, formatTime)
import qualified Data.UUID as UUID
import Data.Vector (Vector)
import qualified Data.Vector as V
import Database.Clickhouse.Conversion.Bytestring.To
import Database.Clickhouse.Conversion.Types
import Database.Clickhouse.Types

-- | Renderer of prepared query with question marks (?) placeholders in place of values.
data ValuesRenderer :: Type

type PreparedQuery = RenderQueryType ValuesRenderer

instance QueryRenderer ValuesRenderer where
  newtype RenderQueryType ValuesRenderer = PreparedQuery {unPreparedQuery :: BSL.ByteString}
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
