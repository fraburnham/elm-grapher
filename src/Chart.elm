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
        StringData sd ->
            td [] [ text sd ]

        FloatData fd ->
            td [] [ text (String.fromFloat fd) ]

header : Header -> Html msg -- what is `msg` here? What should it be?
header headerData =
    case headerData of
        RowData hd ->
            tr [] (map column hd)

        DataMissing ->
            tr [] [ text "Missing header data" ]

row : Row -> Html msg
row rowData =
    case rowData of
        RowData rd ->
            map column rd
                |> tr []

        DataMissing ->
            tr [] [ text "Missing data row" ]

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
