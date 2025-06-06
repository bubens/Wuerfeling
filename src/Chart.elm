module Chart exposing (render)

import Dict exposing (Dict)
import Element exposing (Element, html)
import LineChart
import LineChart.Area
import LineChart.Axis
import LineChart.Axis.Intersection
import LineChart.Axis.Line as Line
import LineChart.Axis.Range
import LineChart.Axis.Tick as Tick
import LineChart.Axis.Ticks as Ticks
import LineChart.Axis.Title as Title
import LineChart.Axis.Values
import LineChart.Colors as Colors
import LineChart.Container
import LineChart.Dots
import LineChart.Events
import LineChart.Grid
import LineChart.Interpolation
import LineChart.Junk as Junk
import LineChart.Legends
import LineChart.Line
import Svg
import Svg.Attributes


guiColor =
    Colors.grayLight


customTick : Int -> Tick.Config msg
customTick n =
    Tick.custom
        { position = toFloat n
        , color = guiColor
        , width = 3
        , length = 7
        , grid = True
        , direction = Tick.negative
        , label = Just (Junk.label guiColor (String.fromInt n))
        }


customAxisLine : Line.Config msg
customAxisLine =
    Line.custom
        (\_ axisRange ->
            { color = guiColor
            , width = 5
            , events = []
            , start = axisRange.min
            , end = axisRange.max
            }
        )


customTitle : Float -> Float -> String -> Title.Config msg
customTitle offsetX offsetY title =
    Title.custom
        (\_ { max } -> max)
        offsetX
        offsetY
        (Svg.g
            [ Svg.Attributes.color "rgb(192, 232, 249)"
            , Svg.Attributes.fontWeight "200"
            , Svg.Attributes.fontSize "20"
            ]
            [ Junk.label Colors.rust title ]
        )


render : Int -> Int -> Dict Int Int -> Element msg
render w h results =
    let
        lenResults =
            Dict.size results
    in
    LineChart.viewCustom
        { x =
            LineChart.Axis.custom
                { title =
                    customTitle
                        -30
                        18
                        "Augen"
                , variable = Just << toFloat << Tuple.first
                , pixels = w
                , range =
                    LineChart.Axis.Range.custom
                        (\{ min, max } ->
                            { min = min
                            , max = max + max / 30
                            }
                        )
                , axisLine =
                    customAxisLine
                , ticks = Ticks.intCustom lenResults customTick
                }
        , y =
            LineChart.Axis.custom
                { title =
                    customTitle
                        30
                        0
                        "Anzahl"
                , variable = Just << toFloat << Tuple.second
                , pixels = h
                , range =
                    LineChart.Axis.Range.custom
                        (\{ max } ->
                            { min = 0
                            , max = max + max / 10
                            }
                        )
                , axisLine =
                    customAxisLine
                , ticks =
                    Ticks.custom
                        (\range _ ->
                            LineChart.Axis.Values.int
                                (LineChart.Axis.Values.around 5)
                                { range | min = 0 }
                                |> List.map customTick
                        )
                }
        , container =
            --LineChart.Container.spaced "line-chart" 40 40 40 80
            LineChart.Container.custom
                { attributesHtml =
                    []
                , attributesSvg =
                    [ Svg.Attributes.fontSize "16"
                    , Svg.Attributes.fontWeight "200"
                    , Svg.Attributes.fontFamily "Sniglet"
                    , Svg.Attributes.color "rgb(192, 232, 249)"
                    ]
                , size = LineChart.Container.static
                , margin = LineChart.Container.Margin 40 40 40 80
                , id = "line-chart"
                }
        , interpolation =
            LineChart.Interpolation.default
        , intersection =
            LineChart.Axis.Intersection.default
        , legends =
            LineChart.Legends.none
        , events =
            LineChart.Events.default
        , junk =
            Junk.default
        , grid =
            LineChart.Grid.lines
                0.3
                guiColor
        , area =
            LineChart.Area.default
        , line =
            LineChart.Line.wider
                5.0
        , dots =
            LineChart.Dots.default
        }
        [ LineChart.line Colors.rust LineChart.Dots.circle "Würfel" <| Dict.toList results ]
        |> html
