module Chart exposing (render)

import Chart.Prepare exposing (Data, Row, Rows)
import Chart.Scale as Scale exposing (Bound, Bounds)
import Html exposing (Html)
import List exposing (map)
import String
import Svg exposing (circle, svg)
import Svg.Attributes exposing (..)
import Tuple exposing (first, second)


row : Row -> Html msg
row rowData =
    circle
        [ cx (String.fromFloat (first rowData))
        , cy (String.fromFloat (second rowData))
        , r "2"
        ]
        []


rows : Rows -> List (Html msg)
rows rowsData =
    map row rowsData


render : Data -> Html msg
render chartData =
    let
        scaledData =
            Scale.data (Bounds (Bound 0.0 300.0) (Bound 0.0 300)) chartData
    in
    -- put the svg in a table? div?
    -- so that the header data can be placed on each axis
    -- should also have regular marks so that it is easy to
    -- tell what is where
    svg
        [ width "300"
        , height "300"
        , viewBox "0 0 305 305"
        ]
        (rows scaledData.rows)
