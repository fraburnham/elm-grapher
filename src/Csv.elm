module Csv exposing (Data, Rows, Row(..), Header, Column(..), parse)

import List exposing (head, tail, map, member, length)
import Maybe exposing (withDefault)
import String

{--
 We're going to do our own csv parsing. Not using a csv library is a bad pattern in real life!
 However, the goal is to learn how to think about types. So we want to put in the low-level
 effort to do so.

 In the real world there are a _ton_ of gotchas regarging csv parsing. Never do it yourself if
 you don't need to.
--}

type Column 
    = StringCol String
    | FloatCol Float
    | ColumnMissing

type Row
    = RowData (List Column)
    | RowIncomplete (List Column) -- This is separately accounted for from ColumnMissing because it gives the caller some flexibility in how to display/render the issue
    | RowMissing
    
type alias Header = Row -- header is just a special case of Row

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
                |> map StringCol
                |> RowData

        [] ->
            RowMissing

floatColumn : Maybe Float -> Column
floatColumn columnData =
    case columnData of
        Just data ->
            FloatCol data

        Nothing ->
            ColumnMissing

rowLength : Row -> Int
rowLength rowData =
    case rowData of
        RowData rd ->
            length rd

        RowIncomplete rd ->
            length rd

        RowMissing ->
            0
                
row : Int -> String -> Row
row headerColumnCount rawRow =
    let
        rowData =
            String.split "," rawRow
                |> map String.toFloat
                |> map floatColumn
    in
        if member ColumnMissing rowData then
            RowIncomplete rowData
        -- this else if doesn't use rowLength because `rowData` is a `List Column` until this fn returns it as a `Row`
        else if length rowData /= headerColumnCount then
            RowIncomplete rowData
        else
            RowData rowData

rows : Int -> String -> Rows
rows headerColumnCount raw =
    case String.split "\n" raw of
        headerRow :: rowsData ->
            map (row headerColumnCount) rowsData

        [] ->
            []

parse : String -> Data
parse raw =
    let
        headerData = header raw
    in
    Data headerData (rows (rowLength headerData) raw)
