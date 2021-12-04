module Main exposing (..)

import Browser
import Chart
import Csv
import File exposing (File)
import File.Select as Select
import Html exposing (Html, button, div, text, input)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onClick, onInput)
import String
import Task


-- fix indentation in elm-mode
-- may need to clobber the indentation settings for other "IDEs"
-- Need a styleguide or something. The indentation style elm-mode is providing has some magic I don't understand
-- install elm-format (there is some fuckery w/ npm i -g elm-format)


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { csvData : Maybe Csv.Data -- This is a maybe so that we can pattern match to display the right stuff
    }

init : () -> (Model, Cmd Msg)
init _ =
    ( Model Nothing, Cmd.none )


type Msg
    = RequestFile
    | FileSelected File
    | DataLoaded String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        RequestFile ->
            ( model
            , Select.file ["text/csv"] FileSelected
            )

        FileSelected file ->
            ( model
            , Task.perform DataLoaded (File.toString file)
            )

        DataLoaded data ->
            ( { model | csvData = Just (Csv.parse data) }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    case model.csvData of
        Nothing ->
            button [ onClick RequestFile ] [ text "Load CSV" ]

        Just data ->
            Chart.render data


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
