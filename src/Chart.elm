-- eventually a place to put the types and fns that will be used to render charts
-- for now the fns to render a csv as a table

module Chart exposing (..) -- TODO: only expose what is needed to make the intended interface clear!

import Csv exposing (Data, Header, Rows, Row)
import Html exposing (Html, button, div, text, input)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onClick, onInput)


render : Data -> Html msg
render chartData =
    div [] []
