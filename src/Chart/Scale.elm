module Chart.Scale exposing (Bound, Bounds, data)

import Chart.Prepare exposing (Column, Data, Header, Row, Rows)
import List exposing (filter, map, maximum, minimum, unzip)
import Tuple exposing (first, second)



-- this type may belong somewhere else since I can use it for the chart's size


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


row : ScaleFactors -> Row -> Row
row sf rowData =
    ( column sf.x (first rowData)
    , column sf.y (second rowData)
    )


rows : ScaleFactors -> Rows -> Rows
rows sf rowsData =
    map (row sf) rowsData


data : Bounds -> Data -> Data
data bounds chartData =
    let
        sf =
            scaleFactors bounds chartData.rows
    in
    Data chartData.header (rows sf chartData.rows)
