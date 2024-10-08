module Database.Clickhouse.Client.HTTP.Types where

import Data.Default
import GHC.Generics
import Network.HTTP.Conduit (ResponseTimeout, responseTimeoutDefault)

data ClickhouseHTTPSettings = ClickhouseHTTPSettings
    { clickhouseUrl :: !String
    , port :: !Int
    , responseTimeout :: !ResponseTimeout
    }
    deriving (Generic, Show, Eq)

instance Default ClickhouseHTTPSettings where
    def =
        ClickhouseHTTPSettings
            { clickhouseUrl = "http://localhost"
            , port = 8123
            , responseTimeout = responseTimeoutDefault
            }