module Chart exposing (render)

import Chart.Prepare exposing (Data, Row, Rows)
import Chart.Scale as Scale exposing (Bound, Bounds)
import Html exposing (Html, div)
import Html.Attributes as HtmlAttr
import List exposing (map)
import String
import Svg exposing (circle, svg)
import Svg.Attributes as SvgAttr
import Tuple exposing (first, second)


row : Row -> Html msg
row rowData =
    circle
        [ SvgAttr.cx (String.fromFloat (first rowData))
        , SvgAttr.cy (String.fromFloat (second rowData))
        , SvgAttr.r "2"
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
    div [ HtmlAttr.style "padding" "10px" ]
        [ svg
            [ SvgAttr.width "300"
            , SvgAttr.height "300"
            , SvgAttr.viewBox "0 0 305 305"
            , SvgAttr.style "border: 1px solid"
            ]
            (rows scaledData.rows)
        ]
