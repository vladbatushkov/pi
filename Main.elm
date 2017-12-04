module Main exposing (..)

import Html exposing (..)


main : Program Never Model Msg
main =
    program { init = model, update = update, view = view, subscriptions = subscription }


type alias Model =
    { name : String }


type Msg
    = NoOp


model : ( Model, Cmd Msg )
model =
    ( Model "Vlad", Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( Model "", Cmd.none )


subscription : Model -> Sub Msg
subscription model =
    Sub.none


view : Model -> Html Msg
view model =
    div [] [ text model.name ]
