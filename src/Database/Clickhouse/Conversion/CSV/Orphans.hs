{-# OPTIONS_GHC -Wno-orphans #-}

module Database.Clickhouse.Conversion.CSV.Orphans where

import qualified Data.Aeson as Aeson
import Data.Csv
import Data.DoubleWord
import Data.IP
import Data.Time
import Data.UUID
import qualified Data.UUID as UUID
import Data.Word
import GHC.TypeNats
import Numeric.Decimal

instance ToField Int128 where
    toField = toField . show

instance ToField Word128 where
    toField = toField . show

instance ToField IPv4 where
    toField = toField . show

instance ToField IPv6 where
    toField = toField . show

instance (Integral a, KnownNat s) => ToField (Decimal r s a) where
    toField = toField . show

instance ToField UTCTime where
    toField = toField . formatTime defaultTimeLocale "%0Y-%m-%d %H:%M:%S"

instance ToField Day where
    toField = toField . formatTime defaultTimeLocale "%0Y-%m-%d"

instance ToField UUID where
    toField = toField . UUID.toString

instance ToField Aeson.Value where
    toField = toField . Aeson.encode

instance ToField Bool where
    toField True = toField (1 :: Word8)
    toField False = toField (0 :: Word8)