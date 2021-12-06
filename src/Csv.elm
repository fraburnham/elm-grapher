module Csv exposing (Data, Rows, Row(..), Header, Column, ColumnData(..), ColumnName, parse)

import List exposing (head, tail, map, map2, member, length)
import Maybe exposing (withDefault)
import String

type alias ColumnName = String

type ColumnData
    = StringCol String
    | FloatCol Float
    | MissingColData

type alias Column =
    { name : ColumnName
    , data : ColumnData
    }

type Row
    = RowData (List Column)
    | RowIncomplete (List Column) -- This is separately accounted for from ColumnMissing because it gives the caller some flexibility in how to display/render the issue
    
type alias Header = List ColumnName

type alias Rows = List Row -- maybe make this the same shape as Row

type alias Data =
    { header : Header
    , rows : Rows
    }


header : String -> Header
header raw =
    case (String.split "\n" raw) of
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
    case rowData of
        RowData rd ->
            length rd

        RowIncomplete rd ->
            length rd

row : Header -> String -> Row
row headerData rawRow =
    let
        headerColumnCount =
            length headerData

        rowData =
            map2 (String.toFloat >> floatColumn) (String.split "," rawRow) headerData
    in
        if member MissingColData (map .data rowData) then
            RowIncomplete rowData
        else if length rowData /= headerColumnCount then
            RowIncomplete rowData
        else
            RowData rowData

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
        headerData = header raw
    in
    Data headerData (rows headerData raw)

