module Main exposing (..)

import Browser
import Chart
import Chart.Prepare as Prepare
import Csv
import File exposing (File)
import File.Select as Select
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (placeholder)
import Html.Events exposing (onClick, onInput)
import Task


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type Render
    = NoData
    | MakeSelections
    | ReadyToRender


type alias Model =
    { csvData : Csv.Data
    , xCol : String
    , yCol : String
    , render : Render
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model (Csv.Data [] []) "" "" NoData
    , Cmd.none
    )


type Msg
    = RequestFile
    | FileSelected File
    | DataLoaded String
    | XColUpdated String
    | YColUpdated String
    | DoRender


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RequestFile ->
            ( model
            , Select.file [ "text/csv" ] FileSelected
            )

        FileSelected file ->
            ( model
            , Task.perform DataLoaded (File.toString file)
            )

        XColUpdated x ->
            ( { model | xCol = x }
            , Cmd.none
            )

        YColUpdated y ->
            ( { model | yCol = y }
            , Cmd.none
            )

        DataLoaded data ->
            ( { model | csvData = Csv.parse data, render = MakeSelections }
            , Cmd.none
            )

        DoRender ->
            -- needs error handling to make sure both xCol and yCol are set
            ( { model | render = ReadyToRender }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    case model.render of
        NoData ->
            button [ onClick RequestFile ] [ text "Load CSV" ]

        MakeSelections ->
            div []
                [ input
                    [ onInput XColUpdated
                    , placeholder "Column to use for x axis"
                    ]
                    []
                , input
                    [ onInput YColUpdated
                    , placeholder "Column to use for y axis"
                    ]
                    []
                , button [ onClick DoRender ] [ text "Render" ]
                ]

        ReadyToRender ->
            Chart.render (Prepare.data ( model.xCol, model.yCol ) model.csvData)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
