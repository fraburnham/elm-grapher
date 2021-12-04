-- eventually a place to put the types and fns that will be used to render charts
-- for now the fns to render a csv as a table

module Chart exposing (render)

import Csv exposing (..) -- (Data, Header, Rows, Row, Column)
import Html exposing (Html, table, tr, td, text)
import List exposing (map)
import String


column : Column -> Html msg
column columnData =
    case columnData of
        StringCol sd ->
            td [] [ text sd ]

        FloatCol fd ->
            td [] [ text (String.fromFloat fd) ]

        ColumnMissing ->
            td [] [ text "Missing column data" ]

header : Header -> Html msg -- what is `msg` here? What should it be?
header headerData =
    case headerData of
        RowData hd ->
            tr [] (map column hd)

        RowMissing ->
            tr [] [ text "Missing header data" ]

        RowIncomplete rd ->
            tr [] [ text "Missing some header columns" ]

row : Row -> Html msg
row rowData =
    case rowData of
        RowData rd ->
            map column rd
                |> tr []

        RowMissing ->
            tr [] [ text "Missing row data" ]

        RowIncomplete rd ->
            tr [] [ text "Missing some data columns" ]

rows : Rows -> List (Html msg)
rows rowsData =
    map row rowsData

render : Data -> Html msg
render chartData =
    let
        renderedData =
            (header chartData.header) :: (rows chartData.rows)
    in
    table [] renderedData
