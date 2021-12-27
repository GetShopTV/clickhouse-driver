{-# LANGUAGE StandaloneKindSignatures #-}

module Database.Clickhouse.Conversion.Types where

import Control.Exception
import Data.Data
import Data.Kind
import Database.Clickhouse.Types

class ToClickhouseType a where
  toClickhouseType :: a -> ClickhouseType

class FromClickhouseType a where
  fromClickhouseType :: ClickhouseType -> a

type QueryRenderer :: Type -> Constraint
class QueryRenderer renderer where
  data RenderQueryType renderer :: Type
  renderRow :: (Foldable row) => RenderQueryType renderer -> row ClickhouseType -> Query
  renderRow query params = renderRows query [params]
  renderRows :: (Functor f, Foldable f, Foldable row) => RenderQueryType renderer -> f (row ClickhouseType) -> Query

data WrongParamsCount = WrongParamsCount deriving (Show, Typeable)

instance Exception WrongParamsCount