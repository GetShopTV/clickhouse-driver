{-# LANGUAGE QuasiQuotes #-}

module Main where

import Conduit
import CustomQQ
import Data.Aeson qualified as Aeson
import Data.Default
import Data.Maybe
import Data.Vector
import Database.Clickhouse.Client.HTTP.Client (ClientHTTP)
import Database.Clickhouse.Client.Types
import Database.Clickhouse.Conversion.CSV.Decode
import Database.Clickhouse.Conversion.CSV.Renderer
import Database.Clickhouse.Conversion.Types
import PyF

chHttpSettings :: ClickhouseConnectionSettings ClientHTTP
chHttpSettings = def{password = "password"}

testInsertCSVQuery :: String -> CSVQuery
testInsertCSVQuery tbl =
    CSVQuery
        [fmt|
INSERT
INTO {tbl}
FORMAT CSV
|]

json :: Aeson.Value
json =
    fromJust . Aeson.decode $
        [customFmt|\
{"name":"Muhammad Ishaq","gender":"Male","age":23,"address":{"street":"87","city":"Gultari Matyal Skardu","state":"Gilgit Baltistan","postalCode":"16350"},"phoneNumber":[{"type":"personal","number":"116263747"}]}
|]

testInsertQueryVals :: [ClickhouseType]
testInsertQueryVals =
    [ ClickInt32 1234
    , ClickJSON json
    ]

testInsertQuery :: String -> [[ClickhouseType]] -> Query
testInsertQuery tbl = renderRows (testInsertCSVQuery tbl)

showQuery :: Query -> IO ()
showQuery q = do
    t <- runConduit $ runQuery q .| sinkLazy
    print t

runQueryInsert :: Query -> IO ()
runQueryInsert q =
    runConduitRes $
        sendSource chHttpSettings q
            .| mapM_C (liftIO . print)

insertsMain :: IO ()
insertsMain = do
    {-     let queryJSON = testInsertQuery "tst_tbl" [testInsertQueryVals]
        showQuery queryJSON
        runQueryInsert queryJSON -}

    {- let vals =
            [ [ClickString "test1\\\\\ \\" \\ \"", ClickInt32 300]
            ] -}
    let vals =
            [ [ClickString "hello worldy\n \t", ClickInt32 100]
            , [ClickString "another string", ClickInt32 200]
            ]
    let queryTxtMultiple = testInsertQuery "strtbl" vals
    showQuery queryTxtMultiple
    runQueryInsert queryTxtMultiple

runQuerySelect :: MonadUnliftIO m => Query -> m [Vector ClickhouseType]
runQuerySelect q =
    runConduitRes $
        sendSource chHttpSettings q
            .| decodeToClickhouseRowsC
            .| sinkList

selectsMain :: IO ()
selectsMain = do
    let selectQ =
            [fmt|\
SELECT * FROM strtbl
|]
    print =<< runQuerySelect selectQ

main :: IO ()
main = do
    --insertsMain
    selectsMain