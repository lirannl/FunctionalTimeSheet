module Main exposing (main)

import Browser exposing (Document)
import Html exposing (button, div, text, textarea)
import Html.Events exposing (onClick, onInput)
import Platform.Cmd exposing (Cmd)


main : Program () State Msg
main =
    Browser.document { init = init, update = update, view = view, subscriptions = subscriptions }


type Msg
    = TextChanged String
    | SwitchTest


update : Msg -> State -> ( State, Cmd Msg )
update msg model =
    case msg of
        TextChanged newval ->
            ( { model | textInput = newval }, Cmd.none )

        SwitchTest ->
            ( { model | test = not model.test }, Cmd.none )


type alias State =
    { textInput : String, test : Bool }


init : () -> ( State, Cmd Msg )
init _ =
    ( { textInput = "", test = True }, Cmd.none )


subscriptions : State -> Sub Msg
subscriptions model =
    case model of
        _ ->
            Sub.none


view : State -> Document Msg
view { textInput, test } =
    { title = "Timesheet App"
    , body =
        [ text "Hello world!"
        , button [ onClick SwitchTest ]
            [ text
                (if test then
                    "Hide"

                 else
                    "Show"
                )
            ]
        , textarea [ onInput (\newval -> TextChanged newval) ] []
        , if test then
            text ("Input: " ++ textInput)

          else
            div [] []
        ]
    }
