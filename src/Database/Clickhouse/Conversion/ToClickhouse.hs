{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.Clickhouse.Conversion.ToClickhouse where

import Data.Binary
import Data.CaseInsensitive (CI)
import Data.CaseInsensitive qualified as CI
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Time (Day, UTCTime)
import Data.UUID (UUID)
import Database.Clickhouse.Client.Types
import Database.Clickhouse.Conversion.Bytestring.From

class ToClickhouseType a where
  toClickhouseType :: a -> ClickhouseType

instance ToClickhouseType TypeValuePairBS where
  toClickhouseType TypeValuePairBS{..} = bsToClickhouseType strType strValue

instance (ToClickhouseType a) => ToClickhouseType (CI a) where
  toClickhouseType = toClickhouseType @a . CI.original

instance (ToClickhouseType a) => ToClickhouseType (Maybe a) where
  toClickhouseType Nothing = ClickNullable Nothing
  toClickhouseType (Just a) = ClickNullable . Just $ toClickhouseType a

instance ToClickhouseType Text where
  toClickhouseType txt = ClickString $ cs txt

instance ToClickhouseType Day where
  toClickhouseType = ClickDate

instance ToClickhouseType UTCTime where
  toClickhouseType = ClickDateTime

instance ToClickhouseType Bool where
  toClickhouseType = ClickBool

instance ToClickhouseType Double where
  toClickhouseType = ClickFloat64

instance ToClickhouseType Float where
  toClickhouseType = ClickFloat32

instance ToClickhouseType UUID where
  toClickhouseType = ClickUUID

instance ToClickhouseType Word8 where
  toClickhouseType = ClickUInt8

instance ToClickhouseType Word32 where
  toClickhouseType = ClickUInt32