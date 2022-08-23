{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Database.Clickhouse.Conversion.CSV.Decode where

import Conduit
import Data.ByteString (ByteString)
import Data.Csv
import Data.Csv.Conduit (fromCsv)
import Data.Functor
import Data.Vector (Vector)
import Data.Vector qualified as V
import Database.Clickhouse.Client.Types
import Database.Clickhouse.Conversion.Bytestring.From

decodeToClickhouseRowsC :: (Monad m) => ConduitM ByteString (Vector ClickhouseType) m ()
decodeToClickhouseRowsC =
  runExceptC
    ( exceptC (mapC id $> Right ()) -- Escaping
        .| fromCsv @(Vector ByteString) defaultDecodeOptions NoHeader -- decoding
        .| do
          _colNames' <- await
          colTypes' <- await
          case colTypes' of
            Just !colTypes -> do
              let toClickhouseType' :: ByteString -> ByteString -> ClickhouseType
                  toClickhouseType' !strType !strValue = bsToClickhouseType strType strValue
                  converters = V.map toClickhouseType' colTypes
              mapC (V.zipWith ($) converters)
            _ -> error $ "Failed due to returned CSV row count lesser than at least 2 (column names and types). Error: " <> show _colNames'
    )
    >>= \case
      Left cpe -> error $ "Failed to decode CSV: " <> show cpe
      Right _ -> pure ()
