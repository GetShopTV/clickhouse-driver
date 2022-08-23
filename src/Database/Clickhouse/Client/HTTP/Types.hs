module Database.Clickhouse.Client.HTTP.Types where

import Data.Default
import GHC.Generics

data ClickhouseHTTPSettings = ClickhouseHTTPSettings
  { clickhouseUrl :: !String
  , port :: !Int
  }
  deriving (Generic, Show, Eq)

instance Default ClickhouseHTTPSettings where
  def =
    ClickhouseHTTPSettings
      { clickhouseUrl = "http://localhost"
      , port = 8123
      }