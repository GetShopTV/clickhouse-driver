{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}


module Database.Clickhouse.Types where

import           Data.ByteString.Char8
import           Data.Default
import           Data.DoubleWord
import           Data.Int
import           Data.Proxy
import           Data.String.Conversions
import           Data.Text
import           Data.Time
import           Data.UUID
import           Data.Vector
import           Data.Word
import           GHC.Generics
import           GHC.TypeLits
import           Network.HTTP.Req

class ToClickhouse a where
  toClick :: a -> Either Text ClickhouseType

type Field = (ByteString, Either Text ClickhouseType)

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

data ClickhouseSettings = ClickhouseSettings
  { scheme   :: !Scheme
  , username :: !Text
  , host     :: !Text
  , port     :: !Int
  , password :: !Text
  }
  deriving Generic

instance Default ClickhouseSettings where
  def = ClickhouseSettings  { scheme = Http
                            , username     = "default"
                            , host         = "localhost"
                            , port         = 8123
                            , password     = ""
                            }

