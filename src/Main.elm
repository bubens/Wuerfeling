module Main exposing (main)

import Array exposing (Array)
import Browser exposing (Document)
import Browser.Events exposing (onAnimationFrame)
import Browser.Navigation as Navigation
import Chart
import Dice
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html exposing (Html)
import Html.Attributes
import Random
import Url



-- TYPES


type State
    = Idle
    | Running Int Int
    | Paused Int Int
    | Done Int


type Input
    = Empty
    | Valid String
    | Invalid String


type Representation
    = Chart
    | Table


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
    | UpdateDiceThrowInput String
    | StartPauseButtonPressed
    | DiceRolled (List Dice.Face)
    | Frame
    | SwitchRepresentation Representation
    | CancelSimulation



-- MODEL


type alias Simulation =
    { dice : List Dice.Dice
    , state : State
    , results : Dict Int Int
    }


type alias Model =
    { simulations : Dict Int Simulation
    , currentSimulation : Int
    , diceThrowInput : Input
    , title : String
    , currentRepresentation : Representation
    }



-- UTILS


getSimulation : Int -> Dict Int Simulation -> Simulation
getSimulation n simulations =
    if Dict.member n simulations then
        Dict.get n simulations
            |> Maybe.withDefault
                (Simulation
                    [ Dice.create ]
                    Idle
                    (initResults 1)
                )

    else
        Simulation
            [ Dice.create ]
            Idle
            (initResults 1)


setSimulation : Simulation -> Dict Int Simulation -> Dict Int Simulation
setSimulation newSimulation simulations =
    Dict.map
        (\i sim ->
            let
                n =
                    List.length newSimulation.dice
            in
            if i == n then
                newSimulation

            else
                sim
        )
        simulations


toPrecision : Float -> Float -> Float
toPrecision prec x =
    let
        exp =
            10 ^ prec
    in
    (toFloat << round)
        (x * exp)
        / exp


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


orangeColor : Element.Color
orangeColor =
    rgb255 200 100 50


borders : Directions
borders =
    { top = 0
    , right = 0
    , bottom = 0
    , left = 0
    }



-- RANDOM-GENERATOR


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


initSimulations : Dict Int Simulation
initSimulations =
    List.range 1 8
        |> List.map
            (\n ->
                ( n
                , Simulation
                    (List.repeat n Dice.create)
                    Idle
                    (initResults n)
                )
            )
        |> Dict.fromList


init : flags -> Url.Url -> Navigation.Key -> ( Model, Cmd msg )
init _ _ _ =
    ( { simulations = initSimulations
      , currentSimulation = 1
      , diceThrowInput = Valid "1"
      , title = "Würfeling"
      , currentRepresentation = Chart
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
                    if val < 1 then
                        1

                    else if val > 8 then
                        8

                    else
                        round val
            in
            ( { model | currentSimulation = diceCount }, Cmd.none )

        UpdateDiceThrowInput val ->
            let
                input =
                    if String.length val == 0 then
                        Empty

                    else if String.all Char.isDigit val then
                        Valid val

                    else
                        Invalid val
            in
            ( { model | diceThrowInput = input }, Cmd.none )

        StartPauseButtonPressed ->
            let
                currentSimulation =
                    getSimulation model.currentSimulation model.simulations

                updatedCurrentSimulation =
                    case currentSimulation.state of
                        Idle ->
                            case model.diceThrowInput of
                                Valid str ->
                                    let
                                        l =
                                            String.toInt str
                                                |> Maybe.withDefault 0
                                    in
                                    { currentSimulation | state = Running l 0 }

                                _ ->
                                    currentSimulation

                        Running end now ->
                            { currentSimulation | state = Paused end now }

                        Paused end now ->
                            { currentSimulation | state = Running end now }

                        Done end ->
                            { currentSimulation
                                | state = Running end 0
                                , results = initResults (List.length currentSimulation.dice)
                            }
            in
            ( { model
                | simulations =
                    setSimulation
                        updatedCurrentSimulation
                        model.simulations
              }
            , Cmd.none
            )

        DiceRolled faces ->
            let
                currentSimulation =
                    getSimulation model.currentSimulation model.simulations

                newDice =
                    List.map2
                        Dice.roll
                        faces
                        currentSimulation.dice

                sumOfDice =
                    newDice
                        |> List.map Dice.toInt
                        |> List.sum

                newResults =
                    Dict.update
                        sumOfDice
                        (Maybe.map <| (+) 1)
                        currentSimulation.results

                ( rollsExpected, rollsSoFar ) =
                    case currentSimulation.state of
                        Running l i ->
                            ( l, i + 1 )

                        _ ->
                            ( 0, 0 )

                updatedCurrentSimulation =
                    Simulation
                        newDice
                        (Running rollsExpected rollsSoFar)
                        newResults
            in
            ( { model
                | simulations = setSimulation updatedCurrentSimulation model.simulations
              }
            , Cmd.none
            )

        Frame ->
            let
                currentSimulation =
                    getSimulation model.currentSimulation model.simulations
            in
            case currentSimulation.state of
                Running end now ->
                    if now < end then
                        let
                            n =
                                List.length currentSimulation.dice
                        in
                        ( model, Random.generate DiceRolled (nDiceRolls n) )

                    else
                        let
                            updatedCurretSimulation =
                                { currentSimulation | state = Done end }
                        in
                        ( { model
                            | simulations =
                                setSimulation updatedCurretSimulation model.simulations
                          }
                        , Cmd.none
                        )

                _ ->
                    ( model, Cmd.none )

        SwitchRepresentation representation ->
            ( { model | currentRepresentation = representation }, Cmd.none )

        CancelSimulation ->
            let
                currentSimulation =
                    getSimulation model.currentSimulation model.simulations

                updatedCurrentSimulation =
                    { currentSimulation
                        | state = Idle
                        , results = initResults (List.length currentSimulation.dice)
                    }
            in
            ( { model
                | simulations =
                    setSimulation updatedCurrentSimulation model.simulations
              }
            , Cmd.none
            )

        Noop ->
            ( model, Cmd.none )



-- VIEW


viewHeader : Model -> Element msg
viewHeader { title } =
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
    row
        [ width fill
        , padding 20
        , Font.color textColor

        --, Element.explain Debug.todo
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

                --, Element.explain Debug.todo
                ]
                [ viewSlider 2 model
                , viewTextInput 1 model
                , viewCounter 1 model
                , viewButton 1 model
                ]
            ]
        ]


viewCounter : Int -> Model -> Element msg
viewCounter portion model =
    let
        currentSimulation =
            getSimulation model.currentSimulation model.simulations

        counterText =
            case currentSimulation.state of
                Running _ now ->
                    String.fromInt now

                Paused _ now ->
                    String.fromInt now

                Idle ->
                    "___"

                Done end ->
                    String.fromInt end
    in
    column
        [ width <| fillPortion portion
        , height fill
        , alignTop
        , spacingXY 0 4
        ]
        [ el
            [ Font.color textColor
            , Font.size 14
            , Region.heading 3
            ]
            (text "Fortschritt:")
        , row
            [ width fill ]
            [ el [ width <| fillPortion 1 ]
                none
            , el
                [ width <| fillPortion 8
                , height <| px 43
                , paddingXY 0 12
                , Font.family [ Font.typeface "Doto", Font.monospace ]
                , Font.bold
                , Font.size 24
                , Font.color darkBackgroundColor
                , Font.letterSpacing 5
                , Font.center
                , Background.color orangeColor
                , Border.color colorColor
                , Border.rounded 5
                , Border.width 2
                ]
                (text counterText)
            , el [ width <| fillPortion 1 ] none
            ]
        ]


viewTextInput : Int -> Model -> Element Msg
viewTextInput portion model =
    Input.text
        [ width <| fillPortion portion
        , Font.color colorColor
        , Border.color
            (case model.diceThrowInput of
                Valid _ ->
                    colorColor

                _ ->
                    rgb255 255 0 0
            )
        , Border.glow
            (rgb255 255 0 0)
            (case model.diceThrowInput of
                Valid _ ->
                    0.0

                _ ->
                    3.0
            )
        , Border.width 2
        , centerX
        , alignTop
        ]
        { onChange =
            UpdateDiceThrowInput
        , text =
            case model.diceThrowInput of
                Empty ->
                    ""

                Valid str ->
                    str

                Invalid str ->
                    str
        , placeholder = Nothing
        , label =
            Input.labelAbove
                [ Font.size 14
                , alignLeft
                , Region.heading 3
                ]
                (el [ centerX ] (text "Anzahl der Würfe:"))
        }


viewSlider : Int -> Model -> Element Msg
viewSlider portion model =
    let
        { dice } =
            getSimulation model.currentSimulation model.simulations

        numberOfDice =
            List.length dice
    in
    Input.slider
        [ width <| fillPortion portion
        , alignTop
        , height fill
        , behindContent
            (el
                [ width fill
                , height (px 4)
                , centerY
                , Background.color colorColor
                , Border.rounded 4
                ]
                none
            )
        ]
        { onChange =
            DiceChanged
        , label =
            Input.labelAbove
                [ alignLeft
                , Font.size 14
                ]
            <|
                row
                    [ Region.heading 3 ]
                    [ el [] (text "Anzahl der Würfe: ")
                    , el [ Font.color orangeColor, Font.bold ] (text <| String.fromInt numberOfDice)
                    ]
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


viewButton : Int -> Model -> Element Msg
viewButton portion model =
    let
        { state } =
            getSimulation model.currentSimulation model.simulations

        buttonText =
            case state of
                Idle ->
                    "Start!"

                Running _ _ ->
                    "Pause!"

                Paused _ _ ->
                    "Weiter!"

                Done _ ->
                    "Nochmal!"
    in
    column
        [ width <| fillPortion portion
        , spacing 3
        ]
        [ Input.button
            [ height <| px 40
            , width fill
            , centerX
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
        , el
            [ Font.size 12
            , pointer
            , centerX
            , Font.variant Font.smallCaps
            , Events.onClick CancelSimulation
            ]
            (text "(Simulation abbrechen)")
        ]


viewDice : Model -> Element msg
viewDice model =
    let
        { dice } =
            getSimulation model.currentSimulation model.simulations
    in
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


viewTable : Int -> Int -> Model -> Element msg
viewTable w h model =
    let
        { results, state } =
            getSimulation model.currentSimulation model.simulations

        cellWidth =
            65

        role =
            \r ->
                Html.Attributes.attribute "role" r
                    |> htmlAttribute

        cellAttributes =
            \rl ->
                [ role rl
                , height fill
                , centerX
                ]

        columnAttributes =
            \cw rl ->
                [ role rl
                , width cw
                , alignTop
                , Border.color textColor
                , Border.width 1
                , spacing 12
                , padding 6
                ]

        columns =
            Dict.foldr
                (\k v l ->
                    let
                        max =
                            case state of
                                Idle ->
                                    0.0

                                Paused _ m ->
                                    toFloat m

                                Running _ m ->
                                    toFloat m

                                Done m ->
                                    toFloat m

                        percent =
                            if max == 0 then
                                "0"

                            else
                                (toFloat v / max)
                                    * 100
                                    |> toPrecision 1
                                    |> String.fromFloat
                    in
                    column
                        (columnAttributes (px cellWidth) "column")
                        [ el
                            (List.append
                                [ Font.color orangeColor

                                --, Background.color brightBackgroundColor
                                ]
                                (cellAttributes "columnheader")
                            )
                            (text <| String.fromInt k)
                        , el
                            (cellAttributes "cell")
                            (text <| String.fromInt v)
                        , el
                            (cellAttributes "cell")
                            (text <| percent ++ "%")
                        ]
                        :: l
                )
                []
                results
    in
    el
        [ width <| px w
        , height <| px h
        , centerX
        ]
        (wrappedRow
            [ role "table"
            , height fill
            , centerX
            , centerY
            , Font.size 16
            , paddingXY 0 12
            ]
            (column
                (List.append
                    [ Font.color orangeColor
                    ]
                    (columnAttributes
                        (px <| cellWidth + 20)
                        "column"
                    )
                )
                [ el
                    (cellAttributes "rowheader")
                    (text "Augen")
                , el
                    (cellAttributes "rowheader")
                    (text "Anzahl")
                , el
                    (cellAttributes "rowheader")
                    (text "Prozent")
                ]
                :: columns
            )
        )


viewRepresentation : Float -> Model -> Element Msg
viewRepresentation w model =
    let
        currentSimulation =
            getSimulation model.currentSimulation model.simulations

        representation =
            case model.currentRepresentation of
                Chart ->
                    Chart.render
                        (round (w - 40))
                        (round (w / 3))
                        currentSimulation.results

                Table ->
                    viewTable
                        (round (w - 40))
                        (round (w / 3))
                        model
    in
    column
        [ spacing 6
        , width <| px (round w)
        ]
        [ viewRepresentationSwitch (round w) model
        , representation
        ]


viewRepresentationSwitch : Int -> Model -> Element Msg
viewRepresentationSwitch w { currentRepresentation } =
    let
        borderWidth =
            2

        deadSpace =
            el
                [ width <| px 6
                , height fill
                , Border.widthEach
                    { top = 0
                    , left = 0
                    , right = 0
                    , bottom = borderWidth
                    }
                ]
                none
    in
    row
        [ Font.size 16
        , width <| px (w - 40)
        , centerX
        ]
        [ deadSpace
        , el
            [ Events.onClick <| SwitchRepresentation Chart
            , Background.color
                (if currentRepresentation == Chart then
                    darkBackgroundColor

                 else
                    brightBackgroundColor
                )
            , Border.color textColor
            , Border.widthEach
                { top = borderWidth
                , right = borderWidth
                , left = borderWidth
                , bottom =
                    if currentRepresentation == Chart then
                        0

                    else
                        borderWidth
                }
            , Border.roundEach
                { topLeft = 3
                , topRight = 3
                , bottomLeft = 0
                , bottomRight = 0
                }
            , padding 6
            , pointer
            ]
            (text "Diagramm")
        , el
            [ width <| px 4
            , height fill
            , Border.widthEach
                { top = 0
                , left = 0
                , right = 0
                , bottom = borderWidth
                }
            ]
            none
        , el
            [ Events.onClick <| SwitchRepresentation Table
            , Border.widthEach
                { top = borderWidth
                , right = borderWidth
                , left = borderWidth
                , bottom =
                    if currentRepresentation == Table then
                        0

                    else
                        borderWidth
                }
            , Border.roundEach
                { topLeft = 3
                , topRight = 3
                , bottomLeft = 0
                , bottomRight = 0
                }
            , Background.color
                (if currentRepresentation == Table then
                    darkBackgroundColor

                 else
                    brightBackgroundColor
                )
            , padding 6
            , pointer
            ]
            (text "Tabelle")
        , el
            [ width fill
            , height fill
            , Border.widthEach
                { top = 0
                , left = 0
                , right = 0
                , bottom = borderWidth
                }
            ]
            none
        ]


view : Model -> Document Msg
view model =
    let
        w =
            1028
    in
    column
        [ width <| px w
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
        [ viewHeader model
        , viewDice model
        , viewRepresentation w model
        , viewForm model
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
    let
        currentSimulation =
            getSimulation model.currentSimulation model.simulations
    in
    case currentSimulation.state of
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
