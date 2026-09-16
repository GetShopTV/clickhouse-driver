{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}

{- |
Shared types of the driver: connection settings, the transport class, the
dynamically typed value representation used by the RowBinary codecs, and the
exceptions thrown by the HTTP transport.
-}
module Database.Clickhouse.Client.Types
  ( -- * Transport class
    ClickhouseClient (..)
  , ClickhouseConnectionSettings (..)
  , defaultConnection
    -- * Requests
  , CHRequest (..)
  , selectRequest
  , externalSelectRequest
  , insertRequest
  , commandRequest
  , effectiveRequestParams
  , settingEnabled
  , ExternalTable (..)
  , externalTable
  , scalarExternal
    -- * Values
  , ClickhouseType (..)
    -- * Exceptions
  , ClickhouseTransportException (..)
  , ClickhouseServerException (..)
  , ClickhouseDecodeException (..)
  , ClickhouseSettingsException (..)
  ) where

import Control.Exception (Exception)
import Control.Monad.Trans.Resource (MonadResource)
import Data.Aeson (Value)
import Data.Conduit (ConduitT)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BSC
import Data.Char (toLower)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.List (isPrefixOf, nubBy)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time (Day, UTCTime)
import Data.UUID (UUID)
import Data.Vector (Vector)
import Data.Word (Word16, Word32, Word64, Word8)
import Data.WideWord (Int128, Int256, Word128, Word256)
import Database.Clickhouse.Conversion.Types (defaultResponseFormat)
import Data.Acquire (Acquire)
import UnliftIO (MonadUnliftIO)

-- | Connection parameters shared by every transport implementation.
data ClickhouseConnectionSettings client = ClickhouseConnectionSettings
  { username :: !Text
  , password :: !Text
  , database :: !Text
  , settings :: ![(String, ClickhouseType)]
  , connectionSettings :: !(ClickhouseClientSettings client)
  }

-- | A sensible default connection: user @default@, empty password, the
-- @default@ database and transport-specific default settings.
defaultConnection :: ClickhouseClientSettings client -> ClickhouseConnectionSettings client
defaultConnection transportSettings =
  ClickhouseConnectionSettings
    { username = "default"
    , password = ""
    , database = "default"
    , settings = []
    , connectionSettings = transportSettings
    }

{- | A transport that can POST a 'CHRequest' to ClickHouse and stream the
response body.

Errors surface as exceptions thrown from the returned conduit:

  * transport failures (curl errors) as 'ClickhouseTransportException';
  * HTTP status >= 400 as 'ClickhouseServerException' carrying the server's
    error text;
  * a failed transfer that is only detected at EOF (truncated body, ...) as a
    'ClickhouseTransportException' when the conduit is drained.
-}
class ClickhouseClient client where
  type ClickhouseClientSettings client = settings | settings -> client

  -- | Stream the response body of a request. The conduit yields raw chunks
  -- and must be fully consumed for the transfer to finish cleanly.
  sendSource ::
    (MonadResource m, MonadUnliftIO m) =>
    ClickhouseConnectionSettings client ->
    CHRequest ->
    ConduitT i ByteString m ()

  -- | 'sendSource' wrapped in an 'Acquire' so it can be embedded into
  -- resource-managed call sites (e.g. Persistent backends).
  sendSourceAcquire ::
    (MonadResource m, MonadUnliftIO m) =>
    ClickhouseConnectionSettings client ->
    CHRequest ->
    Acquire (ConduitT i ByteString m ())
  sendSourceAcquire settings = pure . sendSource settings

-- | A request to the ClickHouse HTTP endpoint.
data CHRequest = CHRequest
  { -- | SQL statement. Sent as the POST body unless 'requestData' is present,
    -- in which case it is sent as the URL @query@ parameter.
    requestSql :: !ByteString
  , -- | RowBinary payload for INSERT statements.
    requestData :: !(Maybe ByteString)
  , -- | Extra URL query parameters (ClickHouse settings, @param_*@, ...).
    requestParams :: ![(ByteString, ByteString)]
  , -- | @X-ClickHouse-Format@ header override.
    requestResponseFormat :: !(Maybe ByteString)
  , -- | Temporary external tables attached to the request as multipart
    -- parts (only meaningful for HTTP).
    requestExternals :: ![ExternalTable]
  }

-- | A temporary table attached to a query as an external data part.
--
-- The SQL must reference the table by 'externalTableName' (e.g.
-- @WHERE id IN (SELECT value FROM ids)@).  Columns are declared with their
-- ClickHouse types; rows are encoded as @RowBinary@, so no textual rendering
-- of values is involved.
data ExternalTable = ExternalTable
  { externalTableName :: !ByteString
  , externalColumns :: ![(ByteString, ByteString)]
  , externalRows :: ![[ClickhouseType]]
  }

-- | Smart constructor for an 'ExternalTable'.
externalTable :: ByteString -> [(ByteString, ByteString)] -> [[ClickhouseType]] -> ExternalTable
externalTable = ExternalTable

-- | A single-value external table with one column named @value@.
scalarExternal :: ByteString -> ByteString -> ClickhouseType -> ExternalTable
scalarExternal name valueType value =
  ExternalTable name [("value", valueType)] [[value]]

-- | A SELECT (or any statement whose RowBinary-with-names response we want
-- to stream and decode).
selectRequest :: ByteString -> CHRequest
selectRequest sql =
  CHRequest
    { requestSql = sql
    , requestData = Nothing
    , requestParams = []
    , requestResponseFormat = Just defaultResponseFormat
    , requestExternals = []
    }

-- | A SELECT with temporary external tables attached (multipart/form-data).
externalSelectRequest :: [ExternalTable] -> ByteString -> CHRequest
externalSelectRequest externals sql =
  CHRequest
    { requestSql = sql
    , requestData = Nothing
    , requestParams = []
    , requestResponseFormat = Just defaultResponseFormat
    , requestExternals = externals
    }

-- | An INSERT whose data payload is streamed from the body. The statement
-- should end with @FORMAT RowBinary@.
insertRequest :: ByteString -> ByteString -> CHRequest
insertRequest statement payload =
  CHRequest
    { requestSql = statement
    , requestData = Just payload
    , requestParams = []
    , requestResponseFormat = Nothing
    , requestExternals = []
    }

-- | A statement whose response body is not interesting (DDL, SET, ...).
commandRequest :: ByteString -> CHRequest
commandRequest sql =
  CHRequest
    { requestSql = sql
    , requestData = Nothing
    , requestParams = []
    , requestResponseFormat = Nothing
    , requestExternals = []
    }

effectiveRequestParams :: [(String, ClickhouseType)] -> CHRequest -> Either ClickhouseSettingsException [(ByteString, ByteString)]
effectiveRequestParams values request = do
  rendered <- mapM renderSetting values
  pure (nubBy (\left right -> fst left == fst right) (requestParams request <> rendered))

renderSetting :: (String, ClickhouseType) -> Either ClickhouseSettingsException (ByteString, ByteString)
renderSetting (name, value)
  | name `elem` ["query", "database", "user", "password", "default_format", "format"] || "param_" `isPrefixOf` name =
      Left (ClickhouseSettingsException ("Use connection fields or requestParams, not settings, for " <> name))
  | otherwise = (Text.encodeUtf8 (Text.pack name),) <$> render value
 where
  invalid = Left (ClickhouseSettingsException ("Setting " <> name <> " requires Bool, String, integer or finite Float"))
  number :: (Integral scalar) => scalar -> Either ClickhouseSettingsException ByteString
  number = Right . BSC.pack . show . toInteger
  floating :: (RealFloat scalar, Show scalar) => scalar -> Either ClickhouseSettingsException ByteString
  floating scalar
    | isNaN scalar || isInfinite scalar = invalid
    | otherwise = Right (BSC.pack (show scalar))
  render scalar = case scalar of
    ClickBool enabled -> Right (if enabled then "1" else "0")
    ClickString bytes -> Right bytes
    ClickInt8 integer -> number integer
    ClickInt16 integer -> number integer
    ClickInt32 integer -> number integer
    ClickInt64 integer -> number integer
    ClickInt128 integer -> number integer
    ClickInt256 integer -> number integer
    ClickUInt8 integer -> number integer
    ClickUInt16 integer -> number integer
    ClickUInt32 integer -> number integer
    ClickUInt64 integer -> number integer
    ClickUInt128 integer -> number integer
    ClickUInt256 integer -> number integer
    ClickFloat32 scalarFloat -> floating scalarFloat
    ClickFloat64 scalarFloat -> floating scalarFloat
    _ -> invalid

settingEnabled :: ByteString -> [(ByteString, ByteString)] -> Bool
settingEnabled name values =
  maybe False (\value -> BSC.map toLower value `elem` ["1", "true"]) (lookup name values)

-- | A dynamically typed ClickHouse value.
--
-- The constructor set matches what the RowBinary codecs support; unknown
-- scalar types are decoded as raw 'ByteString's.
data ClickhouseType
  = ClickString !ByteString
  | -- | @FixedString(n)@: raw bytes without the length prefix.
    ClickFixedString !ByteString
  | ClickBool !Bool
  | ClickInt8 !Int8
  | ClickInt16 !Int16
  | ClickInt32 !Int32
  | ClickInt64 !Int64
  | ClickInt128 !Int128
  | ClickInt256 !Int256
  | ClickUInt8 !Word8
  | ClickUInt16 !Word16
  | ClickUInt32 !Word32
  | ClickUInt64 !Word64
  | ClickUInt128 !Word128
  | ClickUInt256 !Word256
  | ClickFloat32 !Float
  | ClickFloat64 !Double
  | ClickDate !Day
  | ClickDate32 !Day
  | ClickDateTime !UTCTime
  | -- | @DateTime64@. First argument is the fractional precision of the
    -- column, so values round-trip to the exact scaled integer.
    ClickDateTime64 !Int !UTCTime
  | ClickUuid !UUID
  | -- | IPv4 as a host-order 32-bit integer (RowBinary wire: UInt32 LE).
    ClickIPv4 !Word32
  | -- | IPv6 as the 16 network-order bytes (RowBinary writes them verbatim).
    ClickIPv6 !ByteString
  | -- | Decimal columns keep the unscaled integer that sits on the wire.
    -- Width is implied by the constructor (@32@/@64@/@128@/@256@ bits).
    ClickDecimal32 !Integer
  | ClickDecimal64 !Integer
  | ClickDecimal128 !Integer
  | ClickDecimal256 !Integer
  | -- | @JSON@ column; requires explicit @*_binary_*_json_as_string@ settings.
    ClickJSON !Value
  | ClickNullable !(Maybe ClickhouseType)
  | ClickArray !(Vector ClickhouseType)
  | ClickTuple !(Vector ClickhouseType)
  | ClickMap !(Vector (ClickhouseType, ClickhouseType))
  deriving stock (Show, Eq)

-- | A libcurl-level failure (connection refused, timeout, truncated body).
newtype ClickhouseTransportException = ClickhouseTransportException
  { transportMessage :: String
  }
  deriving stock (Show)

instance Exception ClickhouseTransportException

-- | ClickHouse answered with an HTTP error status; the body carries its
-- human-readable error text.
data ClickhouseServerException = ClickhouseServerException
  { serverStatus :: !Int
  , serverMessage :: !ByteString
  }
  deriving stock (Show)

instance Exception ClickhouseServerException

-- | The response body could not be decoded as RowBinary.
newtype ClickhouseDecodeException = ClickhouseDecodeException
  { decodeMessage :: String
  }
  deriving stock (Show)

instance Exception ClickhouseDecodeException

newtype ClickhouseSettingsException = ClickhouseSettingsException String
  deriving stock (Show, Eq)

instance Exception ClickhouseSettingsException
