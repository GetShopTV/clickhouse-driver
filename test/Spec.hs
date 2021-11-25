{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
import           Data.Text                      ( Text )
import Data.Int (Int64)
import Database.Clickhouse.Types
import Data.String.Conversions (cs)

instance (ToClickhouse a) => ToClickhouse (Maybe a) where
  toClick = \case
              Nothing -> pure ClickNull
              Just a -> toClick a

instance ToClickhouse (Text) where
  toClick = pure . ClickString . cs

instance ToClickhouse (Int64) where
  toClick = pure . ClickInt64

data SomeStructure = SomeStructure
  { field1 :: Maybe Text
  , field2 :: Int64
  }


main :: IO ()
main = putStrLn "Test suite not yet implemented"
