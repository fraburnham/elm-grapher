module Chart.Scale exposing (Bound, Bounds, data)

import Chart.Prepare exposing (Column, Data, Row, Rows)
import List exposing (map, maximum, minimum, unzip)
import Tuple exposing (first, second)


type alias Bound =
    { min : Float -- maybe these should be ints since pixels are ints...
    , max : Float
    }


type alias Bounds =
    { x : Bound
    , y : Bound
    }


type alias Scaler =
    Column -> Column


type alias Scalers =
    { x : Scaler
    , y : Scaler
    }


type alias AxisAdjuster =
    Column -> Column


scaler : Bound -> Bound -> Column -> Column
scaler chartBound dataBound colData =
    (((colData - dataBound.min) / (dataBound.max - dataBound.min)) * (chartBound.max - chartBound.min)) + chartBound.min


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


rowBounds : Rows -> Bounds
rowBounds rowsData =
    let
        unzipped =
            unzip rowsData

        xColumns =
            first unzipped

        yColumns =
            second unzipped

        getMax =
            colExtreme maximum

        getMin =
            colExtreme minimum
    in
    Bounds (Bound (getMin xColumns) (getMax xColumns)) (Bound (getMin yColumns) (getMax yColumns))


column : Scaler -> Column -> Column
column colScaler colData =
    -- the fn signature here makes it seem like I'm doing something stupid
    colScaler colData


row : Scalers -> AxisAdjuster -> Row -> Row
row scalers yAdjuster rowData =
    ( column scalers.x (first rowData)
    , yAdjuster (column scalers.y (second rowData))
    )


rows : Scalers -> AxisAdjuster -> Rows -> Rows
rows scalers yAdjuster rowsData =
    map (row scalers yAdjuster) rowsData


data : Bounds -> Data -> Data
data chartBounds chartData =
    let
        yAdjuster =
            yOriginAdjust chartBounds.y

        dataBounds =
            rowBounds chartData.rows

        scalers =
            Scalers (scaler chartBounds.x dataBounds.x) (scaler chartBounds.y dataBounds.y)
    in
    Data chartData.header (rows scalers yAdjuster chartData.rows)
