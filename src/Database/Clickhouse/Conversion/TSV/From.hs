{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Database.Clickhouse.Conversion.TSV.From where

import Control.Applicative
import Control.Monad.Error.Class (MonadError)
import Data.Attoparsec.ByteString.Char8 as AB
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSC
import Data.Char (ord)
import Data.Csv
import qualified Data.Csv as CSV
import Data.Foldable
import Data.Time
import Data.Time.Zones.All (fromTZName, tzByName)
import Data.UUID
import Data.Vector (Vector)
import qualified Data.Vector as V
import Database.Clickhouse.Conversion.Bytestring.From
import Database.Clickhouse.Conversion.ToClickhouse
import Database.Clickhouse.Types
import Debug.Trace
import GHC.Int
import GHC.Word
import Network.HTTP.Req (Scheme (Http), defaultHttpConfig, runReq)
import Replace.Attoparsec.ByteString

-- | ClickHouse use tab-separated csv.
decOpts :: DecodeOptions
decOpts =
  defaultDecodeOptions
    { decDelimiter = fromIntegral (ord '\t')
    }

decodeToClickhouseRows :: ByteString -> (Vector LBS.ByteString, Vector (Vector ClickhouseType))
decodeToClickhouseRows bs =
  {- traceShow bs $ -}
  case decoded of
    Left s -> error $ "Failed to decode TSV, error: " <> s
    Right vec -> do
      -- First two rows are column names and types, they always must exist if table exists.
      if V.length vec < 2
        then error $ "Failed due to returned row count lesser than at least 2 (column names and types): length " <> show (V.length vec)
        else
          let colNames = vec V.! 0 -- column names are not interesting
              colTypes = vec V.! 1
              colData = V.map (V.map LBS.toStrict) . V.drop 2 $ vec
              toClickhouseType' :: ByteString -> ByteString -> ClickhouseType
              toClickhouseType' !strType !strValue = toClickhouseType $ TypeValuePairBS {strType, strValue}
              converters = V.map (toClickhouseType' . LBS.toStrict) colTypes
              convertedData = V.map (V.zipWith ($) converters) colData
           in (colNames, convertedData)
  where
    decoded = decodeWith decOpts NoHeader (preprocess bs)

preprocess :: ByteString -> LBSC.ByteString
preprocess bs = LBSC.cons '\"' $ LBSC.snoc (LBS.fromStrict (escapeTSV bs)) '\"'

escapeTSV :: ByteString -> ByteString
escapeTSV = streamEdit combinedToEscape escaper
  where
    escapeableCharsTSV = char '\t' <|> char '\n' <|> char '\"'
    escapedFromClickhouse = string "\\\\" <|> string "\\\'"
    combinedToEscape = Left <$> escapeableCharsTSV <|> Right <$> escapedFromClickhouse
    escaper (Left c)
      | c == '\t' = "\"\t\""
      | c == '\n' = "\"\n\""
      | c == '\"' = "\"\""
      | otherwise = BSC.singleton c
    escaper (Right str)
      | str == "\\\\" = "\\"
      | str == "\\\'" = "\'"
      | otherwise = str

-- Merged with string character escaping
{- escapeTSV :: ByteString -> ByteString
escapeTSV = streamEdit escapeableChars escaper
  where
    escapeableChars = asum [char '\t', char '\n', char '\"']
    escaper c
      | c == '\t' = "\"\t\""
      | c == '\n' = "\"\n\""
      | c == '\"' = "\"\""
      | otherwise = BSC.singleton c
 -}
