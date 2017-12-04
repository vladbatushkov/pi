module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (href, class, style)
import Round as Math exposing (..)
import Material
import Material.Scheme as Scheme
import Material.Color as Color
import Material.Button as Button
import Material.Options as Options exposing (css)
import Material.Card as Card


white : Options.Property c m
white =
    Color.text Color.white


main : Program Never Model Msg
main =
    program { init = model, update = update, view = view, subscriptions = always Sub.none }


type alias Mdl =
    Material.Model


type alias Model =
    { mdl : Material.Model
    , position : Int
    }


type Msg
    = NoOp
    | Mdl (Material.Msg Msg)


model : ( Model, Cmd Msg )
model =
    ( Model Material.model 1, Cmd.none )


cut : Int -> String
cut position =
    String.left position (toString pi)


next : Int -> String
next position =
    String.right 1 <| cut (position + 1)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mdl msg_ ->
            Material.update Mdl msg_ model

        NoOp ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    card model
        |> Scheme.topWithScheme Color.Red Color.Teal


card : Model -> Html Msg
card model =
    Card.view
        [ css "width" "500px"
        , css "margin-left" "auto"
        , css "margin-right" "auto"
        , css "top" "100px"
        , Color.background (Color.color Color.Amber Color.S500)
        ]
        [ Card.title [] [ Card.head [] [ text "Pi" ] ]
        , Card.text [] [ body model ]
        , Card.actions [ Card.border ] [ text "3,14?" ]
        ]


body : Model -> Html Msg
body model =
    div
        [ style [ ( "text-align", "center" ) ] ]
        [ button 0 "1" Color.BlueGrey model
        , button 1 "2" Color.BlueGrey model
        , button 2 "3" Color.BlueGrey model
        , button 3 "5" Color.BlueGrey model
        ]


button : Int -> String -> Color.Hue -> Model -> Html Msg
button id_ value color_ model =
    Button.render Mdl
        [ id_ ]
        model.mdl
        [ Options.onClick NoOp
        , css "margin" "5px"
        , Color.background (Color.color color_ Color.S500)
        , Button.raised
        , Button.colored
        , Button.ripple
        ]
        [ text value ]
