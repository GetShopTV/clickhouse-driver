{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Database.Clickhouse.Types where

import Control.Monad.Reader
import Data.ByteString.Char8
import qualified Data.ByteString.Lazy as BSL
import Data.Csv
import Data.Default
import Data.DoubleWord
import Data.Int
import Data.Kind
import Data.Proxy
import Data.String.Conversions
import Data.Text
import Data.Time
import Data.UUID
import Data.Vector
import Data.Word
import GHC.Generics
import GHC.TypeLits
import Network.HTTP.Req

newtype Query = Query BSL.ByteString

runQuery :: Query -> ByteString
runQuery (Query bs) = BSL.toStrict bs

class ToClickhouse a where
  toClick :: a -> Either Text ClickhouseType

type Field = (ByteString, Either Text ClickhouseType)

-- | Supported clickhouse types
data ClickhouseType
  = ClickInt8 !Int8
  | ClickInt16 !Int16
  | ClickInt32 !Int32
  | ClickInt64 !Int64
  | ClickInt128 !Int128
  | ClickUInt8 !Word8
  | ClickUInt16 !Word16
  | ClickUInt32 !Word32
  | ClickUInt64 !Word64
  | ClickUInt128 !Word128
  | ClickString !ByteString
  | ClickTuple !(Vector ClickhouseType)
  | ClickArray !(Vector ClickhouseType)
  | ClickDecimal !Float
  | ClickDecimal32 !Float
  | ClickDecimal64 !Double
  | ClickDecimal128 !Double
  | ClickFloat32 !Float
  | ClickFloat64 !Double
  | ClickIPv4 !Word32
  | ClickIPv6 !Word32 !Word32 !Word32 !Word32
  | ClickDate Day
  | ClickDateTime UTCTime
  | ClickUUID UUID
  | ClickNull
  deriving (Show, Eq)

-- | Class of clickhouse clients. Currently only HTTP/HTTPS client available
type ClickhouseClient :: Type -> Constraint
class ClickhouseClient client where
  type ClickhouseClientSettings client = settings | settings -> client
  send :: (MonadIO m) => ClickhouseClientSettings client -> ClickhouseEnv -> Query -> m ByteString

data ClickhouseEnv = ClickhouseEnv
  { username :: !Text,
    password :: !Text,
    dbScheme :: !Text
  }
  deriving (Generic)

instance Default ClickhouseEnv where
  def =
    ClickhouseEnv
      { username = "default",
        password = "",
        dbScheme = "default"
      }
