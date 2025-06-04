module Main exposing (main)

import Browser exposing (Document)
import Browser.Events exposing (onAnimationFrame)
import Browser.Navigation as Navigation
import Chart
import Dice
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html exposing (Html)
import Random
import Url



-- TYPES


type State
    = Idle
    | Running Int Int
    | Paused Int Int


type Input
    = Empty
    | Valid String
    | Invalid String


type alias Document msg =
    { title : String
    , body : List (Html msg)
    }


type alias Directions =
    { top : Int
    , right : Int
    , bottom : Int
    , left : Int
    }


type Msg
    = Noop
    | DiceChanged Float
    | ThrowsChanged String
    | StartPauseButtonPressed
    | DiceRolled (List Dice.Face)
    | Frame



-- MODEL


type alias Model =
    { state : State
    , dice : List Dice.Dice
    , inputThrows : Input
    , results : Dict Int Int
    , title : String
    }



-- UTILS


leftOf : b -> a -> ( a, b )
leftOf right left =
    ( left, right )


withCommand : Cmd Msg -> Model -> ( Model, Cmd Msg )
withCommand =
    leftOf


withTitle : String -> List (Html msg) -> Document msg
withTitle title body =
    Document title body


darkBackgroundColor : Element.Color
darkBackgroundColor =
    rgb255 26 40 31


brightBackgroundColor : Element.Color
brightBackgroundColor =
    rgb255 57 62 70


textColor : Element.Color
textColor =
    rgb255 192 232 249


colorColor : Element.Color
colorColor =
    rgb255 206 123 145


borders : Directions
borders =
    { top = 0
    , right = 0
    , bottom = 0
    , left = 0
    }


nDiceRolls : Int -> Random.Generator (List Dice.Face)
nDiceRolls n =
    Random.list n Dice.generateRandomFace



-- INIT


initResults : Int -> Dict Int Int
initResults dice =
    List.range dice (6 * dice)
        |> List.map
            (\v -> ( v, 0 ))
        |> Dict.fromList


init : flags -> Url.Url -> Navigation.Key -> ( Model, Cmd msg )
init _ _ _ =
    ( { state = Idle
      , dice = List.singleton <| Dice.create
      , inputThrows = Valid "1"
      , results = initResults 1
      , title = "Würfeling"
      }
    , Cmd.none
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DiceChanged val ->
            let
                diceCount =
                    round val
            in
            { model
                | dice = List.repeat diceCount Dice.create
                , results = initResults diceCount
            }
                |> withCommand Cmd.none

        ThrowsChanged val ->
            let
                isEmpty =
                    if String.length val == 0 then
                        True

                    else
                        False

                isNumeric =
                    String.all Char.isDigit val

                input =
                    if isEmpty then
                        Empty

                    else if isNumeric then
                        Valid val

                    else
                        Invalid val
            in
            { model | inputThrows = input }
                |> withCommand Cmd.none

        StartPauseButtonPressed ->
            case model.state of
                Idle ->
                    case model.inputThrows of
                        Valid str ->
                            let
                                l =
                                    String.toInt str
                                        |> Maybe.withDefault 0
                            in
                            { model | state = Running l 0 }
                                |> withCommand Cmd.none

                        _ ->
                            ( model, Cmd.none )

                Running end now ->
                    ( { model | state = Paused end now }, Cmd.none )

                Paused end now ->
                    ( { model | state = Running end now }, Cmd.none )

        DiceRolled faces ->
            let
                newDice =
                    List.map2
                        Dice.roll
                        faces
                        model.dice

                sumOfDice =
                    newDice
                        |> List.map Dice.toInt
                        |> List.sum

                newResult =
                    Dict.update
                        sumOfDice
                        (Maybe.map <| (+) 1)
                        model.results

                ( rollsExpected, rollsSoFar ) =
                    case model.state of
                        Running l i ->
                            ( l, i + 1 )

                        _ ->
                            ( 0, 0 )
            in
            { model
                | dice = newDice
                , results = newResult
                , state = Running rollsExpected rollsSoFar
            }
                |> withCommand Cmd.none

        Frame ->
            case model.state of
                Running l i ->
                    if i < l then
                        let
                            n =
                                List.length model.dice
                        in
                        ( model, Random.generate DiceRolled (nDiceRolls n) )

                    else
                        { model | state = Idle }
                            |> withCommand Cmd.none

                _ ->
                    ( model, Cmd.none )

        Noop ->
            ( model, Cmd.none )



-- VIEW


viewHeader : String -> Element msg
viewHeader title =
    row
        [ Font.size 42
        , Font.color colorColor
        , Font.family [ Font.typeface "Poiret One" ]
        , Font.bold
        , width fill
        , paddingEach
            { borders
                | top = 30
                , right = 30
                , left = 30
                , bottom = 20
            }
        ]
        [ el
            [ centerX
            , width <| fill
            , Border.widthEach
                { borders
                    | bottom = 2
                }
            , Region.heading 1
            ]
            (text title)
        ]


viewForm : Model -> Element Msg
viewForm model =
    let
        numberOfDice =
            List.length model.dice
    in
    row
        [ width fill
        , padding 20
        , Font.color textColor
        ]
        [ column
            [ width fill ]
            [ el
                [ Font.color colorColor
                , Font.family [ Font.typeface "Poiret One" ]
                , Font.bold
                , Font.size 32
                , Border.widthEach
                    { borders
                        | bottom = 2
                    }
                , Region.heading 2
                ]
                (text "Einstellungen")
            , el
                [ height (px 30)
                ]
                none
            , row
                [ width fill
                , height fill
                , spacing 20
                ]
                [ Input.slider
                    [ width <| fillPortion 2
                    , behindContent
                        (el
                            [ width fill
                            , height (px 4)
                            , centerY
                            , Background.color colorColor
                            , Border.rounded 2
                            ]
                            none
                        )
                    ]
                    { onChange =
                        \f ->
                            if model.state /= Idle then
                                Noop

                            else
                                DiceChanged f
                    , label =
                        Input.labelBelow
                            [ centerX
                            , Font.size 18

                            --, Font.family [ Font.monospace ]
                            ]
                            (text <| "Anzahl der Würfel: " ++ String.fromInt numberOfDice)
                    , min = 1.0
                    , max = 8.0
                    , value = toFloat numberOfDice
                    , step = Just 1.0
                    , thumb =
                        Input.thumb
                            [ width <| px 16
                            , height <| px 16
                            , centerY
                            , Background.color brightBackgroundColor
                            , Border.rounded 8
                            ]
                    }
                , Input.text
                    [ width <| fillPortion 1
                    , Font.color colorColor
                    , Border.color
                        (case model.inputThrows of
                            Valid _ ->
                                colorColor

                            _ ->
                                rgb255 255 0 0
                        )
                    , Border.glow
                        (rgb255 255 0 0)
                        (case model.inputThrows of
                            Valid _ ->
                                0.0

                            _ ->
                                3.0
                        )
                    , Border.width 2
                    , centerX
                    ]
                    { onChange =
                        \s ->
                            if model.state /= Idle then
                                Noop

                            else
                                ThrowsChanged s
                    , text =
                        case model.inputThrows of
                            Empty ->
                                ""

                            Valid str ->
                                str

                            Invalid str ->
                                str
                    , placeholder = Nothing
                    , label =
                        Input.labelBelow
                            [ centerX
                            , Font.size 14

                            --, Font.family [ Font.monospace ]
                            , clip
                            ]
                            (column
                                []
                                [ el [ centerX ] (text "Anzahl der Würfe")
                                , el [ centerX ] (text "(1 - 9999)")
                                ]
                            )
                    }
                , viewButton model
                ]
            ]
        ]


viewButton : Model -> Element Msg
viewButton { state } =
    let
        buttonText =
            case state of
                Idle ->
                    "Start!"

                Running _ _ ->
                    "Pause!"

                Paused _ _ ->
                    "Weiter!"
    in
    Input.button
        [ width <| fillPortion 1
        , height <| px 40
        ]
        { onPress =
            Just StartPauseButtonPressed
        , label =
            el
                [ Border.width 2
                , Background.color brightBackgroundColor
                , Border.color colorColor

                --, Font.family [ Font.monospace ]
                , height fill
                , width fill
                ]
                (el
                    [ width shrink
                    , height shrink
                    , centerX
                    , centerY
                    ]
                    (text buttonText)
                )
        }


viewDice : List Dice.Dice -> Element msg
viewDice dice =
    row
        [ height <| px 120
        , width fill
        , padding 20
        , centerX
        ]
        (List.map
            (\d ->
                el
                    [ width shrink
                    , height <| px 80
                    , centerX
                    , centerY
                    ]
                    (html <| Dice.toSvg 80 d)
            )
            dice
        )


view : Model -> Document Msg
view model =
    let
        header =
            viewHeader model.title

        dice =
            viewDice model.dice

        chart =
            Chart.render model.results

        form =
            viewForm model
    in
    column
        [ width <| px 840
        , height <| fill
        , Background.color darkBackgroundColor
        , centerX
        , Border.widthXY 2 0
        , Border.color colorColor
        , Border.glow colorColor 1.0
        , Font.family
            [ Font.typeface "Sniglet"
            , Font.sansSerif
            ]
        , Region.mainContent
        ]
        [ header
        , dice
        , chart
        , form
        ]
        |> Element.layout
            [ Background.color brightBackgroundColor
            , Font.color textColor
            ]
        |> List.singleton
        |> withTitle model.title



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.state of
        Running _ _ ->
            onAnimationFrame <| always Frame

        _ ->
            Sub.none



-- URL CONTROL


urlChange : Url.Url -> Msg
urlChange _ =
    Noop


urlRequest : Browser.UrlRequest -> Msg
urlRequest _ =
    Noop



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = urlChange
        , onUrlRequest = urlRequest
        }
