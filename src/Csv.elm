module Csv exposing (..) --(Data, Rows, Row, Header, Column, parse)
-- What does the expose ened to look like now? I need `StringData` and such
-- to be exposed, but I must be doing it wrong...

import List exposing (head, tail, map)
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
    = StringData String -- these names suck now that they have to be exported. Work out something better
    | FloatData Float

type Row
    = RowData (List Column)
    | DataMissing
    
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
                |> map StringData
                |> RowData

        [] ->
            DataMissing

floatColumn : Maybe Float -> Column
floatColumn columnData =
    -- this is a bad crutch, I think. Should throw a runtime error if there are Maybes since it will cause the data/col count to mismatch
    -- fail fast on broken data
    FloatData (withDefault 0 columnData)
                
row : String -> Row
row rawRow =
    String.split "," rawRow
        |> map String.toFloat
        |> map floatColumn
        |> RowData

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
