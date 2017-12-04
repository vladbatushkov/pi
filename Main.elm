module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (href, class, style)
import Material
import Material.Scheme as Scheme
import Material.Color as Color
import Material.Button as Button
import Material.Options as Options exposing (css)
import Material.Card as Card
import Material.Chip as Chip
import Material.Slider as Slider


main : Program Never Model Msg
main =
    program { init = model, update = update, view = view, subscriptions = always Sub.none }


type alias Mdl =
    Material.Model


type alias Model =
    { mdl : Material.Model
    , position : Int
    , level : Level
    }


type Level
    = Easy
    | Medium
    | Hard


type Msg
    = NoOp
    | SliderMsg Int Float
    | Mdl (Material.Msg Msg)


model : ( Model, Cmd Msg )
model =
    ( Model Material.model 60 Easy, Cmd.none )


pi_ : String
pi_ =
    "3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679"


current : Int -> String
current position =
    String.left position pi_


next : Int -> String
next position =
    String.right 1 <| current (position + 1)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mdl msg_ ->
            Material.update Mdl msg_ model

        NoOp ->
            ( model, Cmd.none )

        SliderMsg idx value ->
            ( { model | level = mapIntToLevel value }, Cmd.none )


mapIntToLevel : Float -> Level
mapIntToLevel value =
    if (value == -1) then
        Easy
    else if (value == 0) then
        Medium
    else
        Hard


mapLevelToFloat : Level -> Float
mapLevelToFloat level =
    case level of
        Easy ->
            -1

        Medium ->
            0

        Hard ->
            1


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
        [ Card.title []
            [ Card.head []
                [ Chip.span []
                    [ Chip.contact Html.span
                        [ Color.background Color.primary
                        , Color.text Color.white
                        ]
                        [ text "Pi" ]
                    , Chip.content []
                        [ text <| current model.position ++ "?" ]
                    ]
                ]
            ]
        , Card.text [] [ body model ]
        , Card.actions
            [ Card.border
            , css "word-wrap" "break-word"
            ]
            [ div [ style [ ( "float", "right" ), ( "margin-right", "20px" ) ] ] [ text <| toString model.level ]
            , div []
                [ Slider.view
                    [ Slider.onChange (SliderMsg 0)
                    , Slider.value <| mapLevelToFloat model.level
                    , Slider.max 1
                    , Slider.min -1
                    , Slider.step 1
                    ]
                ]
            ]
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
