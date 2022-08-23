{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Database.Clickhouse.Client.Types where

import Conduit
import Data.Aeson qualified as Aeson
import Data.ByteString.Builder (stringUtf8)
import Data.ByteString.Char8
import Data.Csv qualified as CSV
import Data.Default
import Data.DoubleWord
import Data.IP
import Data.Int
import Data.String
import Data.Text
import Data.Time
import Data.UUID
import Data.Vector
import Data.Word
import Database.Clickhouse.Conversion.CSV.Orphans ()
import GHC.Generics
import GHC.TypeNats
import Numeric.Decimal (Decimal, RoundToZero)

newtype Query = Query {runQuery :: ConduitM () ByteString IO ()}

instance IsString Query where
  fromString s =
    Query $ yield (stringUtf8 s) .| builderToByteString

-- | Class of clickhouse clients.
class ClickhouseClient client where
  type ClickhouseClientSettings client = settings | settings -> client
  sendSource :: MonadResource m => ClickhouseConnectionSettings client -> Query -> ConduitM i ByteString m ()

-- | Class of clickhouse clients with Acquire interface. Needed for Persistent's SqlBackend.
class ClickhouseClient client => ClickhouseClientAcquire client where
  sendSourceAcquire :: MonadIO m => ClickhouseConnectionSettings client -> Query -> Acquire (ConduitM i ByteString m ())

data ClickhouseConnectionSettings client = ClickhouseConnectionSettings
  { username :: !Text
  , password :: !Text
  , dbScheme :: !Text
  , connectionSettings :: !(ClickhouseClientSettings client)
  }
  deriving (Generic)

instance Default (ClickhouseClientSettings client) => Default (ClickhouseConnectionSettings client) where
  def =
    ClickhouseConnectionSettings
      { username = "default"
      , password = ""
      , dbScheme = "default"
      , connectionSettings = def
      }

-- | Supported clickhouse types
data ClickhouseType where
  ClickNullable :: !(Maybe ClickhouseType) -> ClickhouseType
  ClickString :: !ByteString -> ClickhouseType
  ClickBool :: !Bool -> ClickhouseType
  ClickInt8 :: !Int8 -> ClickhouseType
  ClickInt16 :: !Int16 -> ClickhouseType
  ClickInt32 :: !Int32 -> ClickhouseType
  ClickInt64 :: !Int64 -> ClickhouseType
  ClickInt128 :: !Int128 -> ClickhouseType
  ClickUInt8 :: !Word8 -> ClickhouseType
  ClickUInt16 :: !Word16 -> ClickhouseType
  ClickUInt32 :: !Word32 -> ClickhouseType
  ClickUInt64 :: !Word64 -> ClickhouseType
  ClickUInt128 :: !Word128 -> ClickhouseType
  ClickDecimal32 :: KnownNat s => !(Decimal RoundToZero s Int32) -> ClickhouseType
  ClickDecimal64 :: KnownNat s => !(Decimal RoundToZero s Int64) -> ClickhouseType
  -- | TODO: Use Int128
  ClickDecimal128 :: KnownNat s => !(Decimal RoundToZero s Integer) -> ClickhouseType
  ClickFloat32 :: !Float -> ClickhouseType
  ClickFloat64 :: !Double -> ClickhouseType
  ClickIPv4 :: !IPv4 -> ClickhouseType
  ClickIPv6 :: !IPv6 -> ClickhouseType
  ClickDate :: !Day -> ClickhouseType
  ClickDateTime :: !UTCTime -> ClickhouseType
  ClickUUID :: !UUID -> ClickhouseType
  ClickArray :: !(Vector ClickhouseType) -> ClickhouseType
  ClickTuple :: !(Vector ClickhouseType) -> ClickhouseType
  ClickJSON :: !Aeson.Value -> ClickhouseType

deriving instance Show ClickhouseType

{-
deriving instance Generic ClickhouseType
 -}

instance CSV.ToField ClickhouseType where
  toField = \case
    ClickInt32 n -> CSV.toField n
    ClickString str -> CSV.toField str
    ClickArray _arr -> error "toDo" --squareBrackets (toMultipleStr arr)
    ClickTuple _arr -> error "toDo" -- roundBrackets (toMultipleStr arr)
    ClickFloat32 a -> CSV.toField a
    ClickFloat64 a -> CSV.toField a
    ClickInt8 a -> CSV.toField a
    ClickInt16 a -> CSV.toField a
    ClickInt64 a -> CSV.toField a
    ClickInt128 a -> CSV.toField a
    ClickUInt8 a -> CSV.toField a
    ClickUInt16 a -> CSV.toField a
    ClickUInt32 a -> CSV.toField a
    ClickUInt64 a -> CSV.toField a
    ClickUInt128 a -> CSV.toField a
    ClickDecimal32 a -> CSV.toField a
    ClickDecimal64 a -> CSV.toField a
    ClickDecimal128 a -> CSV.toField a
    ClickIPv4 a -> CSV.toField a
    ClickIPv6 a -> CSV.toField a
    ClickDateTime time -> CSV.toField time
    ClickDate day -> CSV.toField day
    ClickUUID uuid -> CSV.toField uuid
    ClickJSON json -> CSV.toField json
    ClickNullable a -> CSV.toField a
    ClickBool a -> CSV.toField a

{- instance Aeson.FromJSON ClickhouseType where
  parseJSON val = val -}
