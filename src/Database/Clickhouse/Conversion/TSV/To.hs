{-# OPTIONS_GHC -Wno-orphans #-}
module Database.Clickhouse.Conversion.TSV.To where

import Data.Char
import Data.Csv
import Database.Clickhouse.Conversion.Bytestring.To
import Database.Clickhouse.Types

instance ToField ClickhouseType where
  toField = toBS

encOpts :: EncodeOptions
encOpts =
  defaultEncodeOptions
    { encDelimiter = fromIntegral (ord '\t')
    }
