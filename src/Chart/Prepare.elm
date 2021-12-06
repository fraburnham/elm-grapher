module Chart.Prepare exposing (..)

import Csv exposing (Data, Header, Rows, Row(..), Column)
import List exposing (map, indexedMap, filter, member)
import Tuple
import String

-- return a Data that has columns whose name isn't in the selectedColumns list removed
-- this will evole as time goes on to handle flipping columns and such for the render (or the render-er will evolve)

column : List String -> Column -> Column
column selectedColumns columnData =
    columnData

row : List String -> Row -> Row
row selectedColumns rowData =
    rowData

rows : List String -> Rows -> Rows
rows selectedColumns rowsData =
    rowsData

data : List String -> Data -> Data
data selectedColumns csvData =
    csvData
