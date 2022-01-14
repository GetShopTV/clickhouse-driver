module Database.Clickhouse.Conversion.FromClickhouseType where

import Data.ByteString
import Database.Clickhouse.Conversion.Bytestring.To
import Database.Clickhouse.Types

class FromClickhouseType a where
  fromClickhouseType :: ClickhouseType -> a

instance FromClickhouseType ByteString where
  fromClickhouseType = toBS