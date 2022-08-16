{-# LANGUAGE ScopedTypeVariables #-}

module Database.ClickHouse where

import Conduit
import Data.ByteString
import Database.Clickhouse.Client.Types
import Database.Clickhouse.Conversion.Types

executePreparedAcquire ::
  forall client renderer f i m.
  (ClickhouseClientAcquire client, QueryRenderer renderer, Foldable f, MonadIO m) =>
  ClickhouseConnectionSettings client ->
  RenderQueryType renderer ->
  f ClickhouseType ->
  Acquire (ConduitM i ByteString m ())
executePreparedAcquire settings query params =
  sendSourceAcquire @client settings renderedRow
 where
  renderedRow = renderRow @renderer query params

{- executePreparedRows ::
  forall client renderer f row.
  (ClickhouseClient client, QueryRenderer renderer, Foldable row, Foldable f, Functor f) =>
  ClickhouseClientSettings client ->
  ClickhouseConnectionSettings ->
  RenderQueryType renderer ->
  f (row ClickhouseType) ->
  IO ByteString
executePreparedRows settings connection query params =
  send @client settings connection renderedRow
 where
  renderedRow = renderRows @renderer query params -}