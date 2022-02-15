module Database.Clickhouse.Conversion.FromClickhouse where

import Data.ByteString
import Database.Clickhouse.Conversion.Bytestring.To
import Database.Clickhouse.Types

class FromClickhouseType a where
  fromClickhouseType :: ClickhouseType -> a

instance FromClickhouseType ByteString where
  fromClickhouseType = toBS