module Chart exposing (render)

import Chart.Prepare exposing (Data, Header(..), Rows, Row, Column(..))
import Chart.Scale as Scale exposing (Bounds, Bound)
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
    tr [] [ column (first rowData)
          , column (second rowData)
          ]

rows : Rows -> List (Html msg)
rows rowsData =
    map row rowsData

render : Data -> Html msg
render chartData =
    let
        scaledData =
            Scale.data (Bounds (Bound 0.0 300.0) (Bound 0.0 300)) chartData

        renderedData =
            (header scaledData.header) :: (rows scaledData.rows)
    in
    table [] renderedData
