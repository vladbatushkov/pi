module Main exposing (..)

import Random exposing (..)
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
import Material.Toggles as Toggles


main : Program Never Model Msg
main =
    program { init = model, update = update, view = view, subscriptions = always Sub.none }


type alias Mdl =
    Material.Model


type alias Model =
    { mdl : Material.Model
    , correct : Int
    , wrong : Int
    , position : Int
    , level : Level
    , elements : List Element
    }


type alias Element =
    { value : Int
    , action : Msg
    }


type Level
    = Easy
    | Medium
    | Hard


type Msg
    = Mdl (Material.Msg Msg)
    | SliderMsg Int Float
    | ChangeLevelMsg Level
    | GenerateNextPuzzleMsg Level
    | AddElementMsg Int
    | CorrectMsg
    | WrongMsg
    | NoOp


model : ( Model, Cmd Msg )
model =
    update (GenerateNextPuzzleMsg Easy) (Model Material.model 0 0 4 Easy [])


pi_ : String
pi_ =
    "3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679"


current : Int -> String
current position =
    String.left position pi_


next : Int -> Level -> Int
next position level =
    let
        step =
            mapLevelToInt level

        str =
            String.toInt <| String.right step <| current (position + step)
    in
        case str of
            Ok a ->
                a

            Err a ->
                0


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mdl msg_ ->
            Material.update Mdl msg_ model

        SliderMsg idx value ->
            update (GenerateNextPuzzleMsg <| mapFloatToLevel value) model

        ChangeLevelMsg level ->
            update (GenerateNextPuzzleMsg level) model

        GenerateNextPuzzleMsg level ->
            ( { model | elements = [], level = level }
            , Cmd.batch
                [ Random.generate AddElementMsg <| generateValue level
                , Random.generate AddElementMsg <| generateValue level
                ]
            )

        AddElementMsg value ->
            if
                (List.any (\a -> a.value == value) model.elements
                    || (value == next model.position model.level)
                )
            then
                ( model, Random.generate AddElementMsg <| generateValue model.level )
            else if (List.length model.elements == 1) then
                let
                    elements =
                        Element (next model.position model.level) CorrectMsg
                            :: (Element value WrongMsg)
                            :: model.elements

                    resultValue =
                        List.filter (\a -> a.action == WrongMsg) elements
                            |> List.map (\a -> a.value)
                            |> List.sum
                in
                    if (List.any (\a -> a.value == resultValue) model.elements) then
                        ( model, Random.generate AddElementMsg <| generateValue model.level )
                    else
                        ( { model | elements = List.sortBy .value (Element resultValue NoOp :: elements) }
                        , Cmd.none
                        )
            else
                ( { model | elements = (Element value WrongMsg) :: model.elements }
                , Cmd.none
                )

        CorrectMsg ->
            if (isSolved model.position) then
                ( model, Cmd.none )
            else
                update (GenerateNextPuzzleMsg model.level)
                    { model
                        | correct = model.correct + 1
                        , position = model.position + mapLevelToInt model.level
                    }

        WrongMsg ->
            ( { model | wrong = model.wrong + 1 }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


mapFloatToLevel : Float -> Level
mapFloatToLevel value =
    if (value == 1) then
        Easy
    else if (value == 2) then
        Medium
    else
        Hard


mapLevelToInt : Level -> Int
mapLevelToInt level =
    case level of
        Easy ->
            1

        Medium ->
            2

        Hard ->
            3


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
                        [ text "π" ]
                    , Chip.content []
                        [ text <|
                            if (isSolved model.position) then
                                current model.position
                            else
                                current model.position ++ "?"
                        ]
                    ]
                ]
            ]
        , Card.text [ css "text-align" "center" ]
            [ div []
                [ text "One number is sum of two other, left number is next digit in Pi"
                ]
            , div []
                [ text <| "Correct: " ++ toString model.correct ++ " Wrong: " ++ toString model.wrong
                ]
            ]
        , Card.text [] [ body model ]
        , Card.actions
            [ Card.border
            ]
            [ footerToggles model
            ]
        ]


isSolved : Int -> Bool
isSolved value =
    value > 60


footerSlider : Model -> Html Msg
footerSlider model =
    div
        []
        [ div [ style [ ( "float", "right" ), ( "margin-right", "20px" ) ] ] [ text <| toString model.level ]
        , Slider.view
            [ Slider.onChange (SliderMsg 0)
            , Slider.value <| toFloat <| mapLevelToInt model.level
            , Slider.max 3
            , Slider.min 1
            , Slider.step 1
            ]
        ]


footerToggles : Model -> Html Msg
footerToggles model =
    div
        [ style [ ( "text-align", "center" ) ] ]
        [ Toggles.radio Mdl
            [ 0 ]
            model.mdl
            [ Toggles.value <| model.level == Easy
            , Toggles.group "LevelRadioGroup"
            , Toggles.ripple
            , Options.onToggle <| ChangeLevelMsg Easy
            , css "margin-right" "10px"
            ]
            [ text "Easy" ]
        , Toggles.radio Mdl
            [ 1 ]
            model.mdl
            [ Toggles.value <| model.level == Medium
            , Toggles.group "LevelRadioGroup"
            , Toggles.ripple
            , Options.onToggle <| ChangeLevelMsg Medium
            , css "margin-right" "10px"
            ]
            [ text "Medium" ]
        , Toggles.radio Mdl
            [ 2 ]
            model.mdl
            [ Toggles.value <| model.level == Hard
            , Toggles.group "LevelRadioGroup"
            , Toggles.ripple
            , Options.onToggle <| ChangeLevelMsg Hard
            ]
            [ text "Hard" ]
        ]


body : Model -> Html Msg
body model =
    div
        [ style [ ( "text-align", "center" ) ] ]
    <|
        List.map
            (\v -> button v model.mdl)
            model.elements


button : Element -> Material.Model -> Html Msg
button element mdl =
    Button.render Mdl
        [ element.value ]
        mdl
        [ Options.onClick <| element.action
        , Button.raised
        , Button.colored
        , Button.ripple
        , Color.background (Color.color Color.BlueGrey Color.S500)
        , css "margin" "5px"
        ]
        [ text <| toString element.value ]


generateValue : Level -> Generator Int
generateValue level =
    case level of
        Easy ->
            Random.int 0 9

        Medium ->
            Random.int 10 99

        Hard ->
            Random.int 100 999
