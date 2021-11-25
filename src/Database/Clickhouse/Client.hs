{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE OverloadedStrings   #-}

module Database.Clickhouse.Client where

import           Database.Clickhouse.Generic
import           Database.Clickhouse.Types
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Data.ByteString.Char8     (ByteString)
import qualified Data.ByteString.Char8     as BS
import           Data.Coerce
import           Data.DoubleWord
import           Data.Either
import           Data.IP
import           Data.Int
import           Data.Proxy
import           Data.String.Conversions   (cs)
import qualified Data.Text                 as T
import           Data.Time
import qualified Data.UUID                 as UUID
import           Data.Vector               (Vector)
import qualified Data.Vector               as V
import           Data.Word
import           GHC.TypeLits
import           Network.HTTP.Req          as R
import           Network.TunedHTTP
import           Text.URI
import           Data.List.NonEmpty (NonEmpty (..), toList, fromList)

toMultipleStr :: Vector ClickhouseType -> ByteString
toMultipleStr list       = BS.intercalate ", " (toBS <$> V.toList list)

toBS :: ClickhouseType -> ByteString
toBS = \case
  (CKInt32  n  )  -> showBS n
  (CKString str)  -> (quoted . escape) str
  (CKArray  arr)  -> squareBrackets (toMultipleStr arr)
  (CKTuple  arr)  -> roundBrackets (toMultipleStr arr)
  (CKFloat32 a)   -> showBS a
  (CKFloat64 a)   -> showBS a

  (CKInt8 a)      -> showBS a
  (CKInt16 a)     -> showBS a
  (CKInt64 a)     -> showBS a
  (CKInt128 a)    -> showBS a

  (CKUInt8 a)     -> showBS a
  (CKUInt16 a)    -> showBS a
  (CKUInt32 a)    -> showBS a
  (CKUInt64 a)    -> showBS a
  (CKUInt128 a)   -> showBS a

  CKDecimal a     -> showBS a
  CKDecimal32 a   -> showBS a
  CKDecimal64 a   -> showBS a
  CKDecimal128 a  -> showBS a

  CKIPv4 a        -> showBS $ toIPv4w a
  CKIPv6 a b c d  -> showBS $ toIPv6w (a, b, c, d)

  CKDateTime time -> quoted . cs $ formatTime defaultTimeLocale "%0Y-%m-%d %H:%M:%S" time
  CKDate day      -> quoted . cs $ formatTime defaultTimeLocale "%0Y-%m-%d" day
  CKUUID uuid     -> quoted . cs $ UUID.toString uuid
  CKNull          -> "null"


prepareClickhouseURI scheme host port =  mkURIBs $ proto <> host <> ":" <> (cs . show) port
  where proto = case scheme of
              Https ->   "https://"
              Http  ->   "http://"

mkClickHouseRequest :: (MonadHttp m, MonadThrow m) =>  ClickhouseSettings -> ByteString -> m ByteString
mkClickHouseRequest ch@ClickhouseSettings { scheme, username, host, port , password} query = do
  let body = ReqBodyBs query
  liftIO $ BS.putStrLn query
  case scheme of
    Https -> do
      response <- req POST (https $ host) (body) bsResponse (R.port port <> header "X-ClickHouse-User"  (cs username) <> header "X-ClickHouse-Key" (cs password))
      pure (responseBody response)
    Http  -> do
      response <- req POST (http $ host) (body) bsResponse (R.port port <> header "X-ClickHouse-User"  (cs username) <> header "X-ClickHouse-Key" (cs password))
      pure (responseBody response)


-- insertMany

-- | Insert raw fields list
insertMultiple
  :: (MonadHttp m, MonadThrow m)
  => ClickhouseSettings
  -> ByteString
  -> NonEmpty [Field]
  -> m ByteString
insertMultiple settings tableName records = do
  let (errors, valids) = partitionEithers $ toList (traverse (\(tname, edata) -> (tname,) <$> edata ) <$> records)
  errorReport errors
  mkClickHouseRequest settings (query tableName (fromList valids))
  where
    -- | TODO: Подумать, надо ли отправлять валидные записи, если есть хоть одна не валидная. + кидать свой эксепшн, вместо error
    errorReport [] = pure ()
    errorReport errors = error . cs $ "Cannot parse clickhouse-representation" <> T.intercalate "\n" errors
-- | Insert structs with ClickRep instance
insertRecords
  :: (MonadHttp m, MonadThrow m, ClickRep a)
  => ClickhouseSettings
  -> ByteString
  -> NonEmpty a
  -> m ByteString

insertRecords settings tableName recordsObjects =
  insertMultiple settings tableName (toClickRep <$> recordsObjects)


-- Render to INSERT
query :: ByteString -> NonEmpty [(ByteString, ClickhouseType)] -> ByteString
query tableName records =
  "INSERT INTO "
    <> tableName
    <> " "
    <> toClickList (columns records)
    <> " FORMAT Values "
    <> BS.intercalate ", " (toList $ recordsList records)

toClickList :: [ByteString] -> ByteString
toClickList l = "(" <> BS.intercalate ", " l <> ")"

toValues :: [ClickhouseType] -> ByteString
toValues l = toClickList (toBS <$> l)

columns :: NonEmpty [(a, b)] -> [a]
columns (record :| _) = (fst . unzip) record

recordsList :: Functor f => f [(a, ClickhouseType)] -> f ByteString
recordsList records = (toValues . snd . unzip) <$> records

-- Helpers
squareBrackets :: ByteString -> ByteString
squareBrackets str = "["<> str <> "]"

roundBrackets :: ByteString -> ByteString
roundBrackets  str = "("<> str <> ")"

quoted :: ByteString -> ByteString
quoted str = "'" <> str <> "'"

escape :: ByteString -> ByteString
escape = BS.intercalate "\'" . BS.split ('\'')

showBS :: (Show s) => s -> ByteString
showBS = (cs . show)
-- fetchClickHouseResponse
--   :: Katip.LogEnv -- ^ Logger.
--   -> HTTP.Manager -- ^ Clickhouse HTTP manager.
--   -> ClickHouseSettings -- ^ Current clickhouse settings.
--   -> ClickHouseQueryText -- ^ Query text.
--   -> IO HttpClientResponse
-- fetchClickHouseResponse logEnv man ch query = do
--   req <- mkClickHouseRequest ch query
--   resp <- fetchDefaultHttpResponse "ClickHouse" logEnv man req
--   case resp of
--     HttpClientException (ErrorText textWithPass) -> do
--       return . HttpClientException . ErrorText $ Text.replace (Text.dropEnd 3 $ clickHousePassword ch) "***" textWithPass
--     another -> return another

-- fetchClickHouseResponseWithFallback
--   :: Katip.LogEnv -- ^ Logger.
--   -> HTTP.Manager -- ^ ClickHouse HTTP manager.
--   -> ClickHouseSettings -- ^ Primary clickhouse settings.
--   -> ClickHouseSettings -- ^ Secondary clickhouse settings.
--   -> ClickHouseQueryText -- ^ Query text.
--   -> IO HttpClientResponse
-- fetchClickHouseResponseWithFallback logEnv manager ch sch query = runHttpClient
--   (fetchClickHouseResponse logEnv manager ch query
--     `catch` \(SomeException _ex) -> fetchClickHouseResponse logEnv manager sch query)
