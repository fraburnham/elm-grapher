module Csv exposing (..) --(Data, Rows, Row, Header, Column, parse)
-- What does the expose ened to look like now? I need `StringData` and such
-- to be exposed, but I must be doing it wrong...

import List exposing (head, tail, map, member)
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
                
row : String -> Row
row rawRow =
    let
        rowData =
            String.split "," rawRow
                |> map String.toFloat
                |> map floatColumn
    in
        -- check if any members are missing by comparing the count to the header count
        -- and by checking if any are ColumnMissing
        if member ColumnMissing rowData then
            RowIncomplete rowData
        else
            RowData rowData
            

rows : String -> Rows
rows raw =
    case String.split "\n" raw of
        headerRow :: rowsData ->
            map row rowsData

        [] ->
            []

parse : String -> Data
parse raw =
    Data (header raw) (rows raw)
