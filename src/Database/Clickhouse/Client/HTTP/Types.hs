module Database.Clickhouse.Client.HTTP.Types where

import Data.Default
import Data.Text
import GHC.Generics
import Network.HTTP.Req

data ClickhouseHTTPSettings = ClickhouseHTTPSettings
  { scheme :: !Scheme,
    host :: !Text,
    port :: !Int
  }
  deriving (Generic)

instance Default ClickhouseHTTPSettings where
  def =
    ClickhouseHTTPSettings
      { scheme = Http,
        host = "localhost",
        port = 8123
      }
