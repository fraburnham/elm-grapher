module Chart.Prepare exposing (..)

import Csv
import List exposing (map, indexedMap, filter, member)


type Column
    = FloatCol Float
    | MissingColData

type alias RowTuple = (Column, Column)

type Row
    = RowData RowTuple
    | RowIncomplete

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
                [ x, y ] ->
                    RowData (Tuple.pair x y) 

                [] ->
                    RowIncomplete
                a :: rest ->
                    RowIncomplete -- This is really a different type of error...

        Csv.RowIncomplete _ ->
            RowIncomplete

rows : List String -> Csv.Rows -> Rows
rows selectedColumns rowsData =
    map (row selectedColumns) rowsData

data : List String -> Csv.Data -> Data
data selectedColumns csvData = -- tighten up selectedColumns to be a tuple? I want to know only 2 were selected
    Data (header selectedColumns csvData.header) (rows selectedColumns csvData.rows)

    
