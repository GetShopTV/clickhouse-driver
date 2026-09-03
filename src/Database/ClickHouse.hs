{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

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
>   let conn = defaultConnection defaultHTTPSettings
>   runCommand conn "CREATE TABLE IF NOT EXISTS t (n UInt64, s String) ENGINE = Memory"
>   runInsert conn "t" ["n", "s"] [[ClickUInt64 1, ClickString "one"], [ClickUInt64 2, ClickString "two"]]
>   rows <- runQuery conn "SELECT n, s FROM t ORDER BY n"
>   print rows
-}
module Database.ClickHouse
  ( -- * Re-exports
    module Database.Clickhouse.Client.Types
  , module Database.Clickhouse.Client.HTTP.Types
  , module Database.Clickhouse.Conversion.ToClickhouse
  , ClientHTTP
    -- * Queries
  , sourceQuery
  , runQuery
  , runInsert
  , runCommand
  ) where

import Control.Monad.Trans.Resource (MonadResource, runResourceT)
import Data.ByteString (ByteString)
import Data.Conduit (ConduitT, await, awaitForever, runConduit, (.|))
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Database.Clickhouse.Client.HTTP.Client (ClientHTTP)
import Database.Clickhouse.Client.HTTP.Types
import Database.Clickhouse.Client.Types
import Database.Clickhouse.Conversion.Binary.Decode (decodeRowBinaryC)
import Database.Clickhouse.Conversion.Binary.Encode (encodeRows)
import Database.Clickhouse.Conversion.ToClickhouse
import Database.Clickhouse.Conversion.Types (renderInsertStatement)
import UnliftIO (MonadUnliftIO)

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
sourceQuery settings sql =
  sendSource settings (selectRequest sql) .| decodeRowBinaryC

-- | Run a query and collect every row.
runQuery ::
  ClickhouseConnectionSettings ClientHTTP ->
  ByteString ->
  IO (Vector (Vector ClickhouseType))
runQuery settings sql =
  Vector.fromList
    <$> runResourceT (runConduit (sourceQuery settings sql .| collect))

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
runInsert settings table columns rows = do
  let statement = Text.encodeUtf8 (renderInsertStatement table columns)
      payload = encodeRows (map Vector.fromList rows)
  runResourceT $
    runConduit (sendSource settings (insertRequest statement payload) .| drain)

-- | Run a statement and discard its output (DDL, INSERT without collecting
-- the result, SET, ...).  Server side errors still throw.
runCommand ::
  ClickhouseConnectionSettings ClientHTTP ->
  ByteString ->
  IO ()
runCommand settings sql =
  runResourceT $
    runConduit (sendSource settings (commandRequest sql) .| drain)

collect :: Monad m => ConduitT a o m [a]
collect = go []
  where
    go acc = await >>= \case
      Nothing -> pure (reverse acc)
      Just value -> go (value : acc)

drain :: Monad m => ConduitT i o m ()
drain = awaitForever (\_ -> pure ())
