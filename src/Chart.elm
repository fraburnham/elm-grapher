module Chart exposing (render)

import Chart.Prepare exposing (Data, Header(..), Rows, Row(..), Column(..))
import Html exposing (Html, table, tr, td, text)
import List exposing (map)
import String
import Tuple exposing (first, second)


column : Column -> Html msg
column columnData =
    case columnData of
        FloatCol fd ->
            td [] [ text (String.fromFloat fd) ]

        MissingColData ->
            td [] [ text "Missing column data" ]

headerColumn : String -> Html msg
headerColumn headerData =
    td [] [ text headerData ]

header : Header -> Html msg -- what is `msg` here? What should it be?
header headerData =
    case headerData of
        HeaderData hd ->
            tr [] [ headerColumn (first hd)
                  , headerColumn (second hd)
                  ]

        HeaderIncomplete ->
            tr [] [ text "Busted header data" ]

row : Row -> Html msg
row rowData =
    case rowData of
        RowData rd ->
            tr [] [ column (first rd)
                  , column (second rd)
                  ]

        RowIncomplete ->
            tr [] [ text "Missing some data" ]

rows : Rows -> List (Html msg)
rows rowsData =
    map row rowsData

render : Data -> Html msg
render chartData =
    let
        -- before rendering scale the data! I think this fn makes sense since it will end up knowing the bounds of the chart
        renderedData =
            (header chartData.header) :: (rows chartData.rows)
    in
    table [] renderedData
