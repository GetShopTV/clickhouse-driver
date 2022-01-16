{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.Clickhouse.Conversion.ToClickhouse where

import Data.Binary
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Time (Day, UTCTime)
import Data.UUID (UUID)
import Database.Clickhouse.Conversion.Bytestring.From
import Database.Clickhouse.Types

class ToClickhouseType a where
  toClickhouseType :: a -> ClickhouseType

instance ToClickhouseType TypeValuePairBS where
  toClickhouseType TypeValuePairBS {..} = bsToClickhouseType strType strValue

instance (ToClickhouseType a) => ToClickhouseType (CI a) where
  toClickhouseType = toClickhouseType @a . CI.original

instance (ToClickhouseType a) => ToClickhouseType (Maybe a) where
  toClickhouseType Nothing = ClickNull
  toClickhouseType (Just a) = toClickhouseType a

instance ToClickhouseType Text where
  toClickhouseType txt = ClickString $ cs txt

instance ToClickhouseType Day where
  toClickhouseType day = ClickDate day

instance ToClickhouseType UTCTime where
  toClickhouseType time = ClickDateTime time

instance ToClickhouseType Bool where
  toClickhouseType True = ClickUInt8 1
  toClickhouseType False = ClickUInt8 0

instance ToClickhouseType Double where
  toClickhouseType f = ClickFloat64 f

instance ToClickhouseType Float where
  toClickhouseType f = ClickFloat32 f

instance ToClickhouseType UUID where
  toClickhouseType = ClickUUID

instance ToClickhouseType Word8 where
  toClickhouseType n = ClickUInt8 n

instance ToClickhouseType Word32 where
  toClickhouseType n = ClickUInt32 n