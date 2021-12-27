{-# LANGUAGE ScopedTypeVariables #-}

module Database.Clickhouse.Conversion.TSV.Renderer where

import qualified Data.ByteString.Lazy as BSL
import Data.Char (ord)
import Data.Csv
import Data.Foldable
import Database.Clickhouse.Conversion.TSV.To
import Database.Clickhouse.Conversion.TSV.Types
import Database.Clickhouse.Conversion.Types
import Database.Clickhouse.Types

-- | Renderer of query that appends ClickhouseType rows as TSV rows.
type TSVQuery = RenderQueryType TSV

instance QueryRenderer TSV where
  newtype RenderQueryType TSV = TSVQuery {unTSVQuery :: BSL.ByteString}
  renderRows query rows = renderRows' query (toList $ toList <$> rows)

renderRows' :: TSVQuery -> [[ClickhouseType]] -> Query
renderRows' (TSVQuery query) rows = Query $ query <> "\n" <> encode rows
