{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Database.Clickhouse.Conversion.CSV.Renderer where

import Conduit
import Data.ByteString.Lazy qualified as BSL
import Data.Csv
import Data.Foldable
import Data.Kind
import Database.Clickhouse.Client.Types
import Database.Clickhouse.Conversion.Types

data CSV :: Type

-- | Renderer of query that appends ClickhouseType rows as CSV rows.
newtype CSVQuery = CSVQuery {unCSVQuery :: BSL.ByteString}

instance QueryRenderer CSV where
  type RenderQueryType CSV = CSVQuery
  renderRows query rows = renderRows' query (toList $ toList <$> rows)

renderRows' :: CSVQuery -> [[ClickhouseType]] -> Query
renderRows' (CSVQuery query) rows = Query $ do
  sourceLazy query
  sourceLazy $ encodeWith defaultEncodeOptions rows
