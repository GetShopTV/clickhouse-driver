module Database.Clickhouse.Conversion.Bytestring.To where

import Data.Attoparsec.ByteString.Char8 as AB
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Foldable
import Data.IP
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.String.Conversions
import Data.Time
import qualified Data.UUID as UUID
import Data.Vector (Vector)
import qualified Data.Vector as V
import Database.Clickhouse.Types
import Replace.Attoparsec.ByteString

toBS :: ClickhouseType -> ByteString
toBS = \case
  ClickInt32 n -> showBS n
  ClickString str -> (quoted . escapeField) str
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

showBS :: (Show s) => s -> ByteString
showBS = cs . show

toMultipleStr :: Vector ClickhouseType -> ByteString
toMultipleStr list = BSC.intercalate ", " (toBS <$> V.toList list)

toClickList :: [ByteString] -> ByteString
toClickList l = "(" <> BSC.intercalate ", " l <> ")"

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

{- escapeField :: ByteString -> ByteString
escapeField = BSC.intercalate "\'" . BSC.split '\''
 -}

escapeField :: ByteString -> ByteString
escapeField = streamEdit escapeableChars escaper
  where
    escapeableChars = match $ asum [char '\\', char '\'']
    escaper (_bs, ch) = BS.pack ['\\', ch]
