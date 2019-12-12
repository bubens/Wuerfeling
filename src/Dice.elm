module Dice exposing
    ( Dice, Face
    , create
    , roll, rollTo, hold, generateRandomFace, toInt, toString
    , toSvg
    )

{-| This module is a small helper to create, handle and visualize a Dice.


# Definition

@docs Dice, Face


# Create

@docs create


# Handle

@docs roll, rollTo, hold, generateRandomFace, toInt, toString


# Visualize

@docs toSvg

-}

import Array
import Html
import Maybe
import Random
import Svg
import Svg.Attributes as Attributes


{-| Unitype for faces of a dice.
Exported only to be used for type-decleration (e.g. Dice.generateRandomFace)
-}
type Face
    = One
    | Two
    | Three
    | Four
    | Five
    | Six


{-| Definition of a Type Dice.

    Dice 1 False

-}
type alias Dice =
    { face : Face
    , held : Bool
    }


{-| Generator for random rolls.
Generates a random face of the type Face.

    update : Msg -> Model -> ( Model, Msg )
    update msg model =
        case msg of
            RollDice ->
                ( model, Random.generate DiceRolled generateRandomFace )

            DiceRolled face ->
                ( Dice.roll face, Cmd.none )

-}
generateRandomFace : Random.Generator Face
generateRandomFace =
    Random.uniform One [ Two, Three, Four, Five, Six ]


{-| Create a Dice with a predefined face.
A newly created Dice will not be held.

    Dice.create

    -- -> { face = None, held = False } : Dice.Dice

-}
create : Dice
create =
    Dice One False


{-| Roll a dice.
Will only if the dice is not currently held.

    Dice.create
        |> roll Two
    -- -> { face = Dice.Two, held = False } : Dice.Dice

-}
roll : Face -> Dice -> Dice
roll newFace dice =
    if dice.held == False then
        { dice | face = newFace }

    else
        dice


{-| Roll the dice to a certain face.
Specify newFace by Int. Returns Nothing if newFace is not a valid face.

    create
        |> rollTo 2
        |> Maybe.withDefault "Error"

-}
rollTo : Dice -> Int -> Maybe Dice
rollTo dice newFace =
    case newFace of
        1 ->
            Just (roll One dice)

        2 ->
            Just (roll Two dice)

        3 ->
            Just (roll Three dice)

        4 ->
            Just (roll Four dice)

        5 ->
            Just (roll Five dice)

        6 ->
            Just (roll Six dice)

        _ ->
            Nothing


{-| Hold a dice.
A held dice will not roll.

    Dice.create
        |> roll Six
        |> hold True
    -- -> { face = , held = True } : Dice.Dice

-}
hold : Bool -> Dice -> Dice
hold isHeld dice =
    { dice | held = isHeld }


{-| Get dice as Int.
Returns current face of dice as int.

    Dice.create
        |> roll Two
        |> asInt
    -- -> 2 : Int

-}
toInt : Dice -> Int
toInt dice =
    case dice.face of
        One ->
            1

        Two ->
            2

        Three ->
            3

        Four ->
            4

        Five ->
            5

        Six ->
            6


toString : Dice -> String
toString dice =
    case dice.face of
        One ->
            "One"

        Two ->
            "Two"

        Three ->
            "Three"

        Four ->
            "Four"

        Five ->
            "Five"

        Six ->
            "Six"


{-| Get a visual representation of the dice as SVG

    Dice.create
        |> roll Four
        |> toSvg
    -- -> <internals> : Html msg

-}
toSvg : Int -> Dice -> Html.Html msg
toSvg width dice =
    getRequiredDots dice.face
        |> createFaceSvg width dice.held



{- getRequiredDots returns a list containing
   the positions of the dots on a dice' face.
   Like this:

    0     1

    2  3  4

    5     6
-}


getRequiredDots : Face -> List Int
getRequiredDots face =
    case face of
        One ->
            [ 3 ]

        Two ->
            [ 5, 1 ]

        Three ->
            [ 5, 3, 1 ]

        Four ->
            [ 0, 1, 5, 6 ]

        Five ->
            [ 0, 1, 3, 5, 6 ]

        Six ->
            [ 0, 1, 2, 4, 5, 6 ]


createFaceSvg : Int -> Bool -> List Int -> Html.Html msg
createFaceSvg width held dots =
    let
        strWidth =
            String.fromInt width
    in
    createDotsSvg width dots
        |> (::)
            (Svg.rect
                [ Attributes.width strWidth
                , Attributes.height strWidth
                , Attributes.x "0"
                , Attributes.y "0"
                , Attributes.fill (getBgColor held)
                ]
                []
            )
        |> Svg.svg
            [ Attributes.width strWidth
            , Attributes.height strWidth
            , Attributes.viewBox ("0 0 " ++ strWidth ++ " " ++ strWidth)
            , Attributes.style "border: solid 1px #000000; border-radius: 10px"
            ]


createDotsSvg : Int -> List Int -> List (Svg.Svg msg)
createDotsSvg width dots =
    dots
        |> List.map (\x -> getDotCoords x)
        |> List.map
            (\( pcx, pcy ) ->
                let
                    w =
                        toFloat width

                    x =
                        String.fromInt <| round <| (pcx * w)

                    y =
                        String.fromInt <| round <| (pcy * w)

                    r =
                        String.fromInt <| round <| (0.1 * w)
                in
                Svg.circle
                    [ Attributes.cx x
                    , Attributes.cy y
                    , Attributes.r r
                    , Attributes.fill "#000000"
                    ]
                    []
            )


getDotCoords : Int -> ( Float, Float )
getDotCoords position =
    case position of
        0 ->
            ( 0.2, 0.2 )

        1 ->
            ( 0.8, 0.2 )

        2 ->
            ( 0.2, 0.5 )

        3 ->
            ( 0.5, 0.5 )

        4 ->
            ( 0.8, 0.5 )

        5 ->
            ( 0.2, 0.8 )

        6 ->
            ( 0.8, 0.8 )

        _ ->
            ( 0.0, 0.0 )


getBgColor : Bool -> String
getBgColor held =
    if held == True then
        "#787878"

    else
        "#FFFFFF"
