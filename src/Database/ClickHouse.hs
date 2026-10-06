{-# LANGUAGE FlexibleContexts #-}

{- |
Streaming ClickHouse driver.

Transport is hcurl (libcurl multi interface); values travel as @RowBinary@
for INSERT and come back as @RowBinaryWithNamesAndTypes@ (the response
header carries the column names and types, so rows decode into dynamically
typed 'ClickhouseType' vectors without a client-side schema).

Minimal example:

> import Database.ClickHouse
> import Database.Clickhouse.Client.HTTP.Types (defaultHTTPSettings)
> import Database.Clickhouse.Client.Types (ClickhouseType (..))
>
> main :: IO ()
> main = do
>   conn <- connectHTTP defaultHTTPSettings
>   runCommand conn "CREATE TABLE IF NOT EXISTS t (n UInt64, s String) ENGINE = Memory"
>   runInsert conn "t" ["n", "s"] [[ClickUInt64 1, ClickString "one"], [ClickUInt64 2, ClickString "two"]]
>   rows <- runQuery conn "SELECT n, s FROM t ORDER BY n"
>   print rows
-}
module Database.ClickHouse
  ( -- * Re-exports
    module Database.Clickhouse.Client.Types
  , module Database.Clickhouse.Client.HTTP.Types
  , module Database.Clickhouse.Conversion.Text.Escaped
  , module Database.Clickhouse.Conversion.ToClickhouse
  , ClientHTTP
  , ClickhouseHTTPTransport (..)
  , newManagedAgent
  , newHTTPTransport
  , newHTTPTransportWith
  , withHTTPConfig
  , transportConfig
  , connectHTTP
  , connectHTTPWith
    -- * Queries
  , sourceQuery
  , sourceQueryWithParams
  , sourceRequest
  , sourceQueryWithExternals
  , runQuery
  , runQueryWithParams
  , runQueryWithExternals
  , runInsert
  , runCommand
  ) where

import Control.Exception (throwIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (MonadResource, runResourceT)
import Data.ByteString (ByteString)
import Data.Conduit (ConduitT, runConduit, (.|))
import Data.Conduit.Combinators (sinkList, sinkNull)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Database.Clickhouse.Client.HTTP.Client
  ( ClientHTTP
  , ClickhouseHTTPTransport (..)
  , newManagedAgent
  , newHTTPTransport
  , newHTTPTransportWith
  , withHTTPConfig
  , transportConfig
  )
import Database.Clickhouse.Client.HTTP.Types
import Database.Clickhouse.Client.Types
import Database.Clickhouse.Conversion.Binary.Decode (decodeRowBinaryCWithSettings)
import Database.Clickhouse.Conversion.Binary.Encode (encodeRowsWithSettings)
import Database.Clickhouse.Conversion.Text.Escaped
import Database.Clickhouse.Conversion.ToClickhouse
import Database.Clickhouse.Conversion.Types (renderInsertStatement)
import UnliftIO (MonadUnliftIO)

-- | Open a connection: spawn the default managed hcurl agent and wrap the
-- given transport knobs into connection settings with the @default@
-- user/password/database. Override the credentials with record update:
--
-- > conn <- connectHTTP defaultHTTPSettings
-- > let conn' = conn { username = "report", password = "…", database = "analytics" }
connectHTTP ::
  ClickhouseHTTPSettings ->
  IO (ClickhouseConnectionSettings ClientHTTP)
connectHTTP options = do
  transport <- newHTTPTransport options
  pure (defaultConnection transport)

-- | Open a managed connection with optional HTTP policy and application hooks.
connectHTTPWith ::
  ClickhouseHTTPSettings ->
  ClickhouseHTTPConfig ->
  IO (ClickhouseConnectionSettings ClientHTTP)
connectHTTPWith options config = defaultConnection <$> newHTTPTransportWith options config

-- | Stream the rows of a SELECT (or any query) as they arrive.
--
-- The statement is POSTed to the server with the response format set to
-- @RowBinaryWithNamesAndTypes@ and each decoded row is yielded as a
-- 'ClickhouseType' vector.
sourceQuery ::
  (ClickhouseClient client, MonadResource m, MonadUnliftIO m) =>
  ClickhouseConnectionSettings client ->
  ByteString ->
  ConduitT () (Vector ClickhouseType) m ()
sourceQuery conn sql = sourceRequest conn (selectRequest sql)

-- | Stream the rows of a SELECT that binds typed @{name:Type}@ query
-- parameters, e.g. @sourceQueryWithParams conn [(\"p\", ClickUInt64 41)] \"SELECT {p:UInt64} + 1\"@.
--
-- Values travel as escaped text (@param_<name>@ multipart fields, or URL
-- parameters when the request body carries an INSERT payload); unrenderable
-- values fail with 'ClickhouseSettingsException' before the request is sent.
-- See "Database.Clickhouse.Conversion.Text.Escaped".
sourceQueryWithParams ::
  (ClickhouseClient client, MonadResource m, MonadUnliftIO m) =>
  ClickhouseConnectionSettings client ->
  [(ByteString, ClickhouseType)] ->
  ByteString ->
  ConduitT () (Vector ClickhouseType) m ()
sourceQueryWithParams conn params sql =
  sourceRequest conn (selectRequestWithParams params sql)

sourceRequest ::
  (ClickhouseClient client, MonadResource m, MonadUnliftIO m) =>
  ClickhouseConnectionSettings client ->
  CHRequest ->
  ConduitT () (Vector ClickhouseType) m ()
sourceRequest conn request = do
  params <- either (liftIO . throwIO) pure (effectiveRequestParams (settings conn) request)
  sendSource conn request .| decodeRowBinaryCWithSettings params

-- | Stream the rows of a SELECT that references temporary external tables
-- (multipart/form-data, RowBinary payloads).
sourceQueryWithExternals ::
  (ClickhouseClient client, MonadResource m, MonadUnliftIO m) =>
  ClickhouseConnectionSettings client ->
  [ExternalTable] ->
  ByteString ->
  ConduitT () (Vector ClickhouseType) m ()
sourceQueryWithExternals conn externals sql =
  sourceRequest conn (externalSelectRequest externals sql)

-- | Run a query and collect every row.
runQuery ::
  ClickhouseConnectionSettings ClientHTTP ->
  ByteString ->
  IO (Vector (Vector ClickhouseType))
runQuery settings sql =
  Vector.fromList
    <$> runResourceT (runConduit (sourceQuery settings sql .| sinkList))

-- | Run a query with typed @{name:Type}@ bindings attached and collect every
-- row.
runQueryWithParams ::
  ClickhouseConnectionSettings ClientHTTP ->
  [(ByteString, ClickhouseType)] ->
  ByteString ->
  IO (Vector (Vector ClickhouseType))
runQueryWithParams settings params sql =
  Vector.fromList
    <$> runResourceT (runConduit (sourceQueryWithParams settings params sql .| sinkList))

-- | Run a query with external tables attached and collect every row.
runQueryWithExternals ::
  ClickhouseConnectionSettings ClientHTTP ->
  [ExternalTable] ->
  ByteString ->
  IO (Vector (Vector ClickhouseType))
runQueryWithExternals settings externals sql =
  Vector.fromList
    <$> runResourceT (runConduit (sourceQueryWithExternals settings externals sql .| sinkList))

-- | Insert rows into a table.
--
-- The table name is used verbatim (qualified names are fine); the columns
-- are double quoted.  Each row must supply a value for every listed column,
-- in order.
runInsert ::
  ClickhouseConnectionSettings ClientHTTP ->
  Text ->
  [Text] ->
  [[ClickhouseType]] ->
  IO ()
runInsert conn table columns rows = do
  let statement = Text.encodeUtf8 (renderInsertStatement table columns)
  params <- either throwIO pure (effectiveRequestParams (settings conn) (insertRequest statement mempty))
  let payload = encodeRowsWithSettings params (map Vector.fromList rows)
  runResourceT $
    runConduit (sendSource conn (insertRequest statement payload) .| sinkNull)

-- | Run a statement and discard its output (DDL, INSERT without collecting
-- the result, SET, ...).  Server side errors still throw.
runCommand ::
  ClickhouseConnectionSettings ClientHTTP ->
  ByteString ->
  IO ()
runCommand settings sql =
  runResourceT $
    runConduit (sendSource settings (commandRequest sql) .| sinkNull)
