module Chart.Scale exposing (Bound, Bounds, data)

import Chart.Prepare exposing (Column, Data, Row, Rows)
import List exposing (map, maximum, unzip)
import Tuple exposing (first, second)


type alias ScaleFactors =
    { x : Float
    , y : Float
    }


type alias Bound =
    { min : Float -- maybe these should be ints since pixels are ints...
    , max : Float
    }


type alias Bounds =
    { x : Bound
    , y : Bound
    }


type alias AxisAdjuster =
    Column -> Column


yOriginAdjust : Bound -> Column -> Column
yOriginAdjust { max } yCol =
    abs (yCol - max)


colExtreme : (List Float -> Maybe Float) -> List Column -> Float
colExtreme fn cols =
    case fn cols of
        Just fc ->
            fc

        Nothing ->
            0


scaleFactors : Bounds -> Rows -> ScaleFactors
scaleFactors bounds rowsData =
    let
        unzipped =
            unzip rowsData

        xColumns =
            first unzipped

        yColumns =
            second unzipped

        getMax =
            colExtreme maximum

        xMax =
            getMax xColumns

        yMax =
            getMax yColumns
    in
    -- ignoring the possiblity that the chart could start somewhere other
    -- than 0,0 for now
    -- other stuff to consider is giving some margin at the edges of the
    -- chart by subtracting the margin from the bounds
    ScaleFactors (bounds.x.max / xMax) (bounds.y.max / yMax)


column : Float -> Column -> Column
column scaleFactor colData =
    colData * scaleFactor


row : ScaleFactors -> AxisAdjuster -> Row -> Row
row sf yAdjuster rowData =
    ( column sf.x (first rowData)
    , yAdjuster (column sf.y (second rowData))
    )


rows : ScaleFactors -> AxisAdjuster -> Rows -> Rows
rows sf yAdjuster rowsData =
    map (row sf yAdjuster) rowsData


data : Bounds -> Data -> Data
data bounds chartData =
    let
        sf =
            scaleFactors bounds chartData.rows

        yAdjuster =
            yOriginAdjust bounds.y
    in
    Data chartData.header (rows sf yAdjuster chartData.rows)
