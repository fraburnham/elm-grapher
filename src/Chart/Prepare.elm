module Chart.Prepare exposing (..)

import Csv
import List exposing (map, indexedMap, filter, member)


-- Gonna keep the MissingData at the Column level for now to simplify
-- the scaling code
type Column
    = FloatCol Float
    | MissingColData

-- I wonder if this can be a list with only two elements instead so that map keeps working
-- Oh duh. Refactor a row to be a record { x : Column, y : Column }
type alias Row = (Column, Column)

type alias Rows = List Row

type alias HeaderTuple = (String, String)

type Header
    = HeaderData HeaderTuple
    | HeaderIncomplete

type alias Data =
    { header : Header
    , rows : Rows
    }


coerceColumnData : Csv.ColumnData -> Column
coerceColumnData columnData =
    case columnData of
        Csv.FloatCol f ->
            FloatCol f

        Csv.StringCol _ ->
            MissingColData
        Csv.MissingColData ->
            MissingColData

headerColumn : List String -> String -> Bool
headerColumn selectedColumns headerColData =
    member headerColData selectedColumns

header : List String -> Csv.Header -> Header
header selectedColumns headerData =
    case filter (headerColumn selectedColumns) headerData of
        -- this has a bug! It needs to order the tuple correctly
        -- it also needs to account for the incomplete state
        [ x, y ] ->
            HeaderData (Tuple.pair x y)

        [] ->
            HeaderIncomplete
        a :: rest ->
            HeaderIncomplete

column : List String -> Csv.Column -> Bool
column selectedColumns columnData =
    member columnData.name selectedColumns

row : List String -> Csv.Row -> Row
row selectedColumns rowData =
    case rowData of
        Csv.RowData rd ->
            case filter (column selectedColumns) rd |> map (.data >> coerceColumnData) of
                -- this has a bug! It needs to order the tuple correctly
                -- it also needs to account for the incomplete state
                [ x, y ] ->
                    (x, y)

                [] ->
                    (MissingColData, MissingColData)
                a :: rest ->
                    (MissingColData, MissingColData)

        Csv.RowIncomplete _ ->
            (MissingColData, MissingColData)

rows : List String -> Csv.Rows -> Rows
rows selectedColumns rowsData =
    map (row selectedColumns) rowsData

data : List String -> Csv.Data -> Data
data selectedColumns csvData = -- tighten up selectedColumns to be a tuple? I want to know only 2 were selected
    Data (header selectedColumns csvData.header) (rows selectedColumns csvData.rows)

    
