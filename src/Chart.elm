module Chart exposing (render)

import Chart.Prepare exposing (Data, Row, Rows)
import Chart.Scale as Scale exposing (Bound, Bounds)
import Html exposing (Html, div)
import Html.Attributes as HtmlAttr
import List exposing (map)
import String
import Svg exposing (circle, line, svg)
import Svg.Attributes as SvgAttr
import Tuple exposing (first, second)


type DivisionAxis
    = X
    | Y


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


division : Bound -> DivisionAxis -> Int -> Html msg
division bound divAxis offset =
    -- type doesn't feel like the right way for this
    -- I think I'm missing a high order fn option...
    case divAxis of
        X ->
            line
                [ SvgAttr.x1 (String.fromInt offset)
                , SvgAttr.y1 (String.fromFloat bound.min)
                , SvgAttr.x2 (String.fromInt offset)
                , SvgAttr.y2 (String.fromFloat bound.max)
                , SvgAttr.stroke "lightgrey"
                , SvgAttr.strokeDasharray "3 8"
                ]
                []

        Y ->
            line
                [ SvgAttr.x1 (String.fromFloat bound.min)
                , SvgAttr.y1 (String.fromInt offset)
                , SvgAttr.x2 (String.fromFloat bound.max)
                , SvgAttr.y2 (String.fromInt offset)
                , SvgAttr.stroke "lightgrey"
                , SvgAttr.strokeDasharray "3 8"
                ]
                []


makeDivisions : Bound -> DivisionAxis -> Int -> Int -> List (Html msg) -> List (Html msg)
makeDivisions bound divAxis divSpacing offset divs =
    if offset >= floor bound.max then
        divs

    else
        makeDivisions bound divAxis divSpacing (offset + divSpacing) (division bound divAxis offset :: divs)


divisions : Bounds -> Int -> List (Html msg)
divisions { x, y } divCount =
    let
        xDivSpacing =
            round x.max // divCount

        yDivSpacing =
            round y.max // divCount
    in
    makeDivisions x X xDivSpacing xDivSpacing [] ++ makeDivisions y Y yDivSpacing yDivSpacing []


render : Data -> Html msg
render chartData =
    let
        bounds =
            -- one day this will be a param...
            Bounds (Bound 0.0 300.0) (Bound 0.0 300)

        scaledData =
            Scale.data bounds chartData
    in
    div [ HtmlAttr.class "chart-container" ]
        [ svg
            [ SvgAttr.width "300"
            , SvgAttr.height "300"
            , SvgAttr.viewBox "0 0 310 310"
            , SvgAttr.class "chart"
            ]
            (divisions bounds 10 ++ rows scaledData.rows)
        ]
