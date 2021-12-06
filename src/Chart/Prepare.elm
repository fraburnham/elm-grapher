module Chart.Prepare exposing (..)

import Csv exposing (Data, Header, Rows, Row(..), Column)
import List exposing (map, indexedMap, filter, member)
import Tuple
import String


headerColumn : List String -> String -> Bool
headerColumn selectedColumns headerColData =
    member headerColData selectedColumns

header : List String -> Header -> Header
header selectedColumns headerData =
    filter (headerColumn selectedColumns) headerData

column : List String -> Column -> Bool
column selectedColumns columnData =
    member columnData.name selectedColumns

row : List String -> Row -> Row
row selectedColumns rowData =
    -- there should be logic here to update the completeness of a row?
    -- since we're reducing the row may be complete after this...
    case rowData of
        RowData rd ->
            filter (column selectedColumns) rd
                |> RowData

        RowIncomplete rd ->
            filter (column selectedColumns) rd
                |> RowIncomplete

rows : List String -> Rows -> Rows
rows selectedColumns rowsData =
    map (row selectedColumns) rowsData

data : List String -> Data -> Data
data selectedColumns csvData =
    Data (header selectedColumns csvData.header) (rows selectedColumns csvData.rows)
