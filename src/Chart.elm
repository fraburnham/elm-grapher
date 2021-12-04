-- eventually a place to put the types and fns that will be used to render charts
-- for now the fns to render a csv as a table

module Chart exposing (render)

import Csv exposing (Data, Header, Rows, Row)
import Html exposing (Html, table, tr, td, text)
import List exposing (map)
import String


header : Header -> Html msg -- what is `msg` here? What should it be?
header headerData =
    case headerData of
        [] ->
            tr [] [ td [] [ text "No header data found" ] ]

        [ a, b ] ->
            tr [] [ td [] [ text a ]
                  , td [] [ text b ]
                  ]

        a :: rest ->
            tr [] [ td [] [ text "Too much header data found" ] ]

row : Row -> Html msg
row rowData =
    case rowData of
        [] ->
            tr [] [ td [] [ text "No row data found" ] ]

        [ a, b ] ->
            tr [] [ td [] [ text (String.fromFloat a) ]
                  , td [] [ text (String.fromFloat b) ]
                  ]

        a :: rest ->
            tr [] [ td [] [ text "Too much row data found" ] ]           

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
