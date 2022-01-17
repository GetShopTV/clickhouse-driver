{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

import Data.Int (Int64)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Database.Clickhouse.Conversion.ToClickhouse (ToClickhouseType)
import Database.Clickhouse.Types

data SomeStructure = SomeStructure
  { field1 :: Maybe Text,
    field2 :: Int64
  }

main :: IO ()
main = putStrLn "Test suite not yet implemented"
