module Chart exposing (Chart, create, render, setBackgroundColor, setBarColor, setHeight, setTextColor, setWidth, update, updateDatapoints)

import Element exposing (..)
import Element.Background as Background
import Element.Font as Font


type alias DataPoint =
    ( Int, String )


type alias Chart =
    { datapoints : List DataPoint
    , width : Maybe Int
    , height : Maybe Int
    , barColor : Maybe Color
    , backgroundColor : Maybe Color
    , textColor : Maybe Color
    }


init : Chart
init =
    { datapoints = []
    , width = Nothing
    , height = Nothing
    , barColor = Nothing
    , backgroundColor = Nothing
    , textColor = Nothing
    }


create : List String -> Chart
create names =
    let
        datapoints =
            List.map
                (Tuple.pair 0)
                names
    in
    { init | datapoints = datapoints }


updateDatapoints : List String -> Chart -> Chart
updateDatapoints names chart =
    let
        datapoints =
            List.map
                (Tuple.pair 0)
                names
    in
    { chart | datapoints = datapoints }


setWidth : Int -> Chart -> Chart
setWidth width chart =
    { chart | width = Just width }


setHeight : Int -> Chart -> Chart
setHeight height chart =
    { chart | height = Just height }


setBarColor : Color -> Chart -> Chart
setBarColor color chart =
    { chart | barColor = Just color }


setBackgroundColor : Color -> Chart -> Chart
setBackgroundColor color chart =
    { chart | backgroundColor = Just color }


setTextColor : Color -> Chart -> Chart
setTextColor color chart =
    { chart | textColor = Just color }


update : List Int -> Chart -> Chart
update values chart =
    let
        datapoints =
            List.map2
                (\value tuple ->
                    Tuple.mapFirst (\first -> value) tuple
                )
                values
                chart.datapoints
    in
    { chart | datapoints = datapoints }


render : Chart -> Element msg
render chart =
    let
        w =
            chart.width
                |> Maybe.map px
                |> Maybe.withDefault fill

        h =
            chart.height
                |> Maybe.map px
                |> Maybe.withDefault fill

        bgclr =
            chart.backgroundColor
                |> Maybe.withDefault (rgb255 255 255 255)

        barclr =
            chart.barColor
                |> Maybe.withDefault (rgb255 0 180 0)

        fontclr =
            chart.textColor
                |> Maybe.withDefault (rgb255 0 0 0)

        maximumValue =
            List.foldl
                (\t max ->
                    let
                        v =
                            Tuple.first t
                    in
                    if v > max then
                        v

                    else
                        max
                )
                -1
                chart.datapoints

        bars =
            chart.datapoints
                |> List.map
                    (\( value, name ) ->
                        let
                            len =
                                List.length chart.datapoints

                            heightLower =
                                if value == 0 then
                                    px 0

                                else if value == maximumValue then
                                    fill

                                else
                                    fillPortion value

                            heightUpper =
                                if value == 0 then
                                    fill

                                else if value == maximumValue then
                                    px 0

                                else
                                    fillPortion (maximumValue - value)
                        in
                        column
                            [ width <| fillPortion len
                            , height fill
                            , Font.color fontclr
                            , Font.center
                            , alignBottom

                            --, explain Debug.todo
                            ]
                            [ el
                                [ Font.size 16
                                , height heightUpper
                                , Background.color bgclr
                                , width fill
                                , alignBottom
                                , padding 5
                                ]
                                (text name)
                            , el
                                [ Font.size 10
                                , Font.family <| List.singleton Font.monospace
                                , Font.bold
                                , height heightLower
                                , width fill
                                , Background.color barclr
                                , alignBottom
                                ]
                                (text <| String.padLeft 2 ' ' <| String.fromInt value)
                            ]
                    )
    in
    row
        [ width w
        , height h
        , Background.color bgclr
        , spacing 2
        , padding 10

        --, explain Debug.todo
        ]
        bars
