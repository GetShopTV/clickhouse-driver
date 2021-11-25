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
import           Network.TunedHTTP

class ToClickhouse a where
  toClick :: a -> Either Text ClickhouseType

type Field = (ByteString, Either Text ClickhouseType)

data ClickhouseType
  = CKInt8 !Int8
  | CKInt16 !Int16
  | CKInt32 !Int32
  | CKInt64 !Int64
  | CKInt128 !Int128
  | CKUInt8 !Word8
  | CKUInt16 !Word16
  | CKUInt32 !Word32
  | CKUInt64 !Word64
  | CKUInt128 !Word128
  | CKString !ByteString
  | CKTuple !(Vector ClickhouseType)
  | CKArray !(Vector ClickhouseType)
  | CKDecimal !Float
  | CKDecimal32 !Float
  | CKDecimal64 !Double
  | CKDecimal128 !Double

  | CKFloat32 !Float
  | CKFloat64 !Double
  | CKIPv4 !Word32
  | CKIPv6 !Word32 !Word32 !Word32 !Word32
  | CKDate Day
  | CKDateTime UTCTime
  | CKUUID UUID
  | CKNull
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

