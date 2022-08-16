module Database.Clickhouse.Conversion.FromClickhouse where

import Data.ByteString
import qualified Data.Csv as CSV
import Database.Clickhouse.Client.Types

class FromClickhouseType a where
  fromClickhouseType :: ClickhouseType -> a

instance FromClickhouseType ByteString where
  fromClickhouseType = CSV.toField