module Csv exposing (Data, Rows, Row, Header, parse)

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

type alias Row = List Float

type alias Header = List String

type alias Rows = List Row

type alias Data =
    { header : Header
    , rows : Rows
    }

{--
 seeing lots of notes in the docs about using `case` to deconstruct a `List` because it gives
 you (x :: xs). Need to figure out how that pattern looks. Should replace these with that.
--}
header : String -> Header
header raw =
    case head (String.split "\n" raw) of
        Just sr ->
            String.split "," sr

        Nothing ->
            ["", ""] -- have to return a list to end the Maybe madness (there is probably a better pattern to learn here...)

row : String -> Row
row rawRow =
    String.split "," rawRow
        |> map String.toFloat
        |> map (withDefault 0) -- this is a bad crutch, I think. Should throw a runtime error if there are Maybes since it will cause the data/col count to mismatch
           -- fail fast on broken data

rows : String -> Rows
rows raw =
    case tail (String.split "\n" raw) of
        Just rawRows ->
            map row rawRows

        Nothing ->
            [[0,0]] -- could this be `[]`?

parse : String -> Data
parse raw =
    Data (header raw) (rows raw)
