module Csv exposing (Column, ColumnData(..), ColumnName, Data, Header, Row, Rows, parse)

import List exposing (head, length, map, map2, member, tail)
import Maybe exposing (withDefault)
import String


type alias ColumnName =
    String


type ColumnData
    = StringCol String
    | FloatCol Float
    | MissingColData


type alias Column =
    { name : ColumnName
    , data : ColumnData
    }


type alias Row =
    List Column


type alias Header =
    List ColumnName


type alias Rows =
    List Row



-- maybe make this the same shape as Row


type alias Data =
    { header : Header
    , rows : Rows
    }


header : String -> Header
header raw =
    case String.split "\n" raw of
        headerRow :: rowsData ->
            String.split "," headerRow

        [] ->
            []


floatColumn : Maybe Float -> ColumnName -> Column
floatColumn columnData columnName =
    case columnData of
        Just data ->
            Column columnName (FloatCol data)

        Nothing ->
            Column columnName MissingColData



-- may be able to delete this now


rowLength : Row -> Int
rowLength rowData =
    length rowData


row : Header -> String -> Row
row headerData rawRow =
    map2 (String.toFloat >> floatColumn) (String.split "," rawRow) headerData


rows : Header -> String -> Rows
rows headerData raw =
    case String.split "\n" raw of
        headerRow :: rowsData ->
            map (row headerData) rowsData

        [] ->
            []


parse : String -> Data
parse raw =
    let
        headerData =
            header raw
    in
    Data headerData (rows headerData raw)
