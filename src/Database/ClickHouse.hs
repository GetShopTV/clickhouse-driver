{-# LANGUAGE ScopedTypeVariables #-}

module Database.ClickHouse where

import Data.ByteString
import Database.Clickhouse.Conversion.Types
import Database.Clickhouse.Types

executePrepared ::
  forall client renderer f.
  (ClickhouseClient client, QueryRenderer renderer, Foldable f) =>
  ClickhouseClientSettings client ->
  ClickhouseConnectionSettings ->
  RenderQueryType renderer ->
  f ClickhouseType ->
  IO ByteString
executePrepared settings connection query params =
  send @client settings connection renderedRow
  where
    renderedRow = renderRow @renderer query params

executePreparedRows ::
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
    renderedRow = renderRows @renderer query params
