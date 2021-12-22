module Database.Clickhouse.Render where

import Control.Exception (Exception)
import Control.Exception.Base (throw)
import Data.Binary.Put (putByteString, putLazyByteString, runPut)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Data (Typeable)
import Data.IP
import Data.Int
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.String.Conversions (cs)
import Data.Time (defaultTimeLocale, formatTime)
import qualified Data.UUID as UUID
import Data.Vector (Vector)
import qualified Data.Vector as V
import Database.Clickhouse.Types (ClickhouseType (..))

newtype Query = Query BSL.ByteString

runQuery :: Query -> ByteString
runQuery (Query bs) = BSL.toStrict bs

renderParams :: Query -> [ClickhouseType] -> Query
renderParams (Query qry) params =
  let fragments = LC.split '?' qry
   in Query . runPut $ merge fragments params
  where
    merge [x] [] = putLazyByteString x
    merge (x : xs) (y : ys) = putLazyByteString x >> putByteString (toBS y) >> merge xs ys
    merge _ _ = throw WrongParamsCount

data WrongParamsCount = WrongParamsCount deriving (Show, Typeable)

instance Exception WrongParamsCount

toBS :: ClickhouseType -> ByteString
toBS = \case
  ClickInt32 n -> showBS n
  ClickString str -> (quoted . escape) str
  ClickArray arr -> squareBrackets (toMultipleStr arr)
  ClickTuple arr -> roundBrackets (toMultipleStr arr)
  ClickFloat32 a -> showBS a
  ClickFloat64 a -> showBS a
  ClickInt8 a -> showBS a
  ClickInt16 a -> showBS a
  ClickInt64 a -> showBS a
  ClickInt128 a -> showBS a
  ClickUInt8 a -> showBS a
  ClickUInt16 a -> showBS a
  ClickUInt32 a -> showBS a
  ClickUInt64 a -> showBS a
  ClickUInt128 a -> showBS a
  ClickDecimal a -> showBS a
  ClickDecimal32 a -> showBS a
  ClickDecimal64 a -> showBS a
  ClickDecimal128 a -> showBS a
  ClickIPv4 a -> showBS $ toIPv4w a
  ClickIPv6 a b c d -> showBS $ toIPv6w (a, b, c, d)
  ClickDateTime time -> quoted . cs $ formatTime defaultTimeLocale "%0Y-%m-%d %H:%M:%S" time
  ClickDate day -> quoted . cs $ formatTime defaultTimeLocale "%0Y-%m-%d" day
  ClickUUID uuid -> quoted . cs $ UUID.toString uuid
  ClickNull -> "null"

toMultipleStr :: Vector ClickhouseType -> ByteString
toMultipleStr list = BS.intercalate ", " (toBS <$> V.toList list)

toClickList :: [ByteString] -> ByteString
toClickList l = "(" <> BS.intercalate ", " l <> ")"

toValues :: [ClickhouseType] -> ByteString
toValues l = toClickList (toBS <$> l)

columns :: NonEmpty [(a, b)] -> [a]
columns (record :| _) = fst <$> record

recordsList :: Functor f => f [(a, ClickhouseType)] -> f ByteString
recordsList records = toValues . map snd <$> records

-- Helpers
squareBrackets :: ByteString -> ByteString
squareBrackets str = "[" <> str <> "]"

roundBrackets :: ByteString -> ByteString
roundBrackets str = "(" <> str <> ")"

quoted :: ByteString -> ByteString
quoted str = "'" <> str <> "'"

escape :: ByteString -> ByteString
escape = BS.intercalate "\'" . BS.split '\''

showBS :: (Show s) => s -> ByteString
showBS = cs . show