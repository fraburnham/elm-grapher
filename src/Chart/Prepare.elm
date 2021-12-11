module Chart.Prepare exposing (Column, Data, Row, Rows, data)

import Csv
import List exposing (filter, foldr, map, member)


type alias Column =
    -- I wonder if this can be a list with only two elements instead so that map keeps working
    -- Oh duh. Refactor a row to be a record { x : Column, y : Column }
    Float


type alias TupleRow =
    -- temp type to transition from Csv.Row to Row
    ( Csv.ColumnData, Csv.ColumnData )


type alias Row =
    ( Column, Column )


type alias Rows =
    List Row


type alias Header =
    ( String, String )


type alias Data =
    { header : Header
    , rows : Rows
    }


coerceRowData : TupleRow -> Rows -> Rows
coerceRowData rowData result =
    let
        -- may be able to do this w/o the let
        ( x, y ) =
            rowData
    in
    case ( x, y ) of
        ( Csv.FloatCol xf, Csv.FloatCol yf ) ->
            ( xf, yf ) :: result

        ( _, _ ) ->
            result


headerColumn : ( String, String ) -> String -> Bool
headerColumn ( xCol, yCol ) headerColData =
    if xCol == headerColData then
        True

    else if yCol == headerColData then
        True

    else
        False


header : ( String, String ) -> Csv.Header -> Header
header selectedColumns headerData =
    let
        ( xCol, yCol ) =
            selectedColumns
    in
    case filter (headerColumn selectedColumns) headerData of
        [ _, _ ] ->
            ( xCol, yCol )

        _ ->
            ( "Missing header data", "Missing header data" )


column : ( String, String ) -> Csv.Column -> Bool
column ( xCol, yCol ) columnData =
    let
        selectedColumns =
            [ xCol, yCol ]
    in
    member columnData.name selectedColumns


row : ( String, String ) -> Csv.Row -> TupleRow
row selectedColumns rowData =
    let
        ( xCol, _ ) =
            selectedColumns
    in
    case filter (column selectedColumns) rowData of
        [ x, y ] ->
            if x.name == xCol then
                ( x.data, y.data )

            else
                ( y.data, x.data )

        _ ->
            ( Csv.MissingColData, Csv.MissingColData )


rows : ( String, String ) -> Csv.Rows -> Rows
rows selectedColumns rowsData =
    map (row selectedColumns) rowsData
        |> foldr coerceRowData []


data : ( String, String ) -> Csv.Data -> Data
data selectedColumns csvData =
    -- tighten up selectedColumns to be a tuple? I want to know only 2 were selected
    Data (header selectedColumns csvData.header) (rows selectedColumns csvData.rows)
