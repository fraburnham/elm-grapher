module Chart.Prepare exposing (..)

import Csv
import List exposing (foldr, map, indexedMap, filter, member)


-- I wonder if this can be a list with only two elements instead so that map keeps working
-- Oh duh. Refactor a row to be a record { x : Column, y : Column }
type alias Column = Float

-- temp type to transition from Csv.Row to Row
type alias TupleRow = (Csv.ColumnData, Csv.ColumnData)

type alias Row = (Column, Column)

type alias Rows = List Row

type alias Header = (String, String)

type alias Data =
    { header : Header
    , rows : Rows
    }


coerceRowData : TupleRow -> Rows -> Rows
coerceRowData rowData result =
    let
        -- may be able to do this w/o the let
        (x,y) = rowData
    in
    case (x,y) of
        (Csv.FloatCol xf, Csv.FloatCol yf) ->
            (xf,yf) :: result

        (_, _) ->
            result

headerColumn : List String -> String -> Bool
headerColumn selectedColumns headerColData =
    member headerColData selectedColumns

header : List String -> Csv.Header -> Header
header selectedColumns headerData =
    case filter (headerColumn selectedColumns) headerData of
        -- this has a bug! It needs to order the tuple correctly
        -- it also needs to account for the incomplete state
        [ x, y ] ->
            (x, y)

        _ ->
            ("Missing header data", "Missing header data")

column : List String -> Csv.Column -> Bool
column selectedColumns columnData =
    member columnData.name selectedColumns

row : List String -> Csv.Row -> TupleRow
row selectedColumns rowData =
    case filter (column selectedColumns) rowData of
        -- this has a bug! It needs to order the tuple correctly
        -- it also needs to account for the incomplete state
        [ x, y ] ->
            (x.data,y.data)
                
        _ ->
            (Csv.MissingColData, Csv.MissingColData)
                            
rows : List String -> Csv.Rows -> Rows
rows selectedColumns rowsData =
    map (row selectedColumns) rowsData
        |> foldr coerceRowData []

data : List String -> Csv.Data -> Data
data selectedColumns csvData = -- tighten up selectedColumns to be a tuple? I want to know only 2 were selected
    Data (header selectedColumns csvData.header) (rows selectedColumns csvData.rows)

    
