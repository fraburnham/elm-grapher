module Chart exposing (render)

import Chart.Prepare exposing (Data, Header, Rows, Row, Column)
import Chart.Scale as Scale exposing (Bounds, Bound)
import Html exposing (Html, table, tr, td, text)
import List exposing (map)
import String
import Tuple exposing (first, second)


column : Column -> Html msg
column columnData =
    td [] [ text (String.fromFloat columnData) ]

headerColumn : String -> Html msg
headerColumn headerData =
    td [] [ text headerData ]

header : Header -> Html msg -- what is `msg` here? What should it be?
header headerData =
    tr [] [ headerColumn (first headerData)
          , headerColumn (second headerData)
          ]

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
