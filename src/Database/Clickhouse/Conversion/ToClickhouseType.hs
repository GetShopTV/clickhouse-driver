{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.Clickhouse.Conversion.ToClickhouseType where

import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Database.Clickhouse.Conversion.Bytestring.From
import Database.Clickhouse.Types

class ToClickhouseType a where
  toClickhouseType :: a -> ClickhouseType

instance ToClickhouseType TypeValuePairBS where
  toClickhouseType TypeValuePairBS {..} = bsToClickhouseType strType strValue

instance (ToClickhouseType a) => ToClickhouseType (CI a) where
  toClickhouseType = toClickhouseType @a . CI.original