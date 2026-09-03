{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

{- |
Conversion from plain Haskell values to the dynamically typed
'ClickhouseType'.  Rows for 'Database.ClickHouse.runInsert' can then be
written with e.g. @map toClickhouseType@.
-}
module Database.Clickhouse.Conversion.ToClickhouse
  ( ToClickhouseType (..)
  ) where

import Data.ByteString (ByteString)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Data.Time (Day, UTCTime)
import Data.UUID (UUID)
import Data.Word (Word16, Word32, Word64, Word8)
import Database.Clickhouse.Client.Types (ClickhouseType (..))

-- | Values convertible to a ClickHouse column value.
class ToClickhouseType a where
  toClickhouseType :: a -> ClickhouseType

instance ToClickhouseType Text where
  toClickhouseType = ClickString . Text.encodeUtf8

instance ToClickhouseType ByteString where
  toClickhouseType = ClickString

instance ToClickhouseType Bool where
  toClickhouseType = ClickBool

instance ToClickhouseType Int8 where
  toClickhouseType = ClickInt8

instance ToClickhouseType Int16 where
  toClickhouseType = ClickInt16

instance ToClickhouseType Int32 where
  toClickhouseType = ClickInt32

instance ToClickhouseType Int64 where
  toClickhouseType = ClickInt64

instance ToClickhouseType Word8 where
  toClickhouseType = ClickUInt8

instance ToClickhouseType Word16 where
  toClickhouseType = ClickUInt16

instance ToClickhouseType Word32 where
  toClickhouseType = ClickUInt32

instance ToClickhouseType Word64 where
  toClickhouseType = ClickUInt64

instance ToClickhouseType Float where
  toClickhouseType = ClickFloat32

instance ToClickhouseType Double where
  toClickhouseType = ClickFloat64

instance ToClickhouseType Day where
  toClickhouseType = ClickDate

instance ToClickhouseType UTCTime where
  toClickhouseType = ClickDateTime

instance ToClickhouseType UUID where
  toClickhouseType = ClickUuid

instance (ToClickhouseType a) => ToClickhouseType (Maybe a) where
  toClickhouseType = ClickNullable . fmap toClickhouseType
