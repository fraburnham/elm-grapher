module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text, input)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onClick, onInput)

main =
  Browser.sandbox { init = init, update = update, view = view }

type alias Model =
    { filename : String }

init : Model
init =
    { filename = "" }
      
type Msg =
    Change String |
    Reset

-- update fn takes a message and model and returns a model
update : Msg -> Model -> Model
update msg model =
    case msg of
        Change newString ->
            { model | filename = newString }

        Reset ->
            { model | filename = "" }
          
-- Need a styleguide or something. The indentation style elm-mode is providing has some magic I don't understand
-- install elm-format (there is some fuckery w/ npm i -g elm-format)

-- view takes a model and returns an Html and a Msg
view : Model -> Html Msg
view model =
  div []
    [ div [] [ text model.filename ]
    , button [ onClick Reset ] [ text "Reset" ]
    , input [ placeholder "Input filename", value model.filename, onInput Change ] []
    ]
