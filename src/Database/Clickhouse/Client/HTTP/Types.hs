module Database.Clickhouse.Client.HTTP.Types where

import Data.Default
import Data.Text
import GHC.Generics
import Network.HTTP.Req

data ClickhouseSettings = ClickhouseSettings
  { scheme :: !Scheme,
    username :: !Text,
    host :: !Text,
    port :: !Int,
    password :: !Text
  }
  deriving (Generic)

data ClickhouseEnv = ClickhouseEnv
  { settings :: !ClickhouseSettings,
    dbScheme :: !Text
  }
  deriving (Generic)

instance Default ClickhouseSettings where
  def =
    ClickhouseSettings
      { scheme = Http,
        username = "default",
        host = "localhost",
        port = 8123,
        password = ""
      }
