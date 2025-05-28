module Chart exposing (render)

import Dict exposing (Dict)
import Element exposing (Element, html)
import LineChart
import LineChart.Area
import LineChart.Axis
import LineChart.Axis.Intersection
import LineChart.Axis.Line
import LineChart.Axis.Range
import LineChart.Axis.Tick
import LineChart.Axis.Ticks
import LineChart.Axis.Title
import LineChart.Axis.Values
import LineChart.Colors
import LineChart.Container
import LineChart.Dots
import LineChart.Events
import LineChart.Grid
import LineChart.Interpolation
import LineChart.Junk
import LineChart.Legends
import LineChart.Line
import Svg.Attributes


render : Dict Int Int -> Element msg
render results =
    let
        lenResults =
            Dict.size results
    in
    LineChart.viewCustom
        { x =
            LineChart.Axis.custom
                { title =
                    LineChart.Axis.Title.atAxisMax
                        -40
                        30
                        "Augen"
                , variable = Just << toFloat << Tuple.first
                , pixels = 800
                , range = LineChart.Axis.Range.default
                , axisLine =
                    LineChart.Axis.Line.full
                        LineChart.Colors.black
                , ticks = LineChart.Axis.Ticks.int lenResults
                }
        , y =
            LineChart.Axis.custom
                { title =
                    LineChart.Axis.Title.atAxisMax
                        40
                        0
                        "Anzahl"
                , variable = Just << toFloat << Tuple.second
                , pixels = 300
                , range =
                    LineChart.Axis.Range.custom
                        (\{ min, max } ->
                            { min = 0
                            , max = max + max / 10
                            }
                        )
                , axisLine =
                    LineChart.Axis.Line.full
                        LineChart.Colors.black
                , ticks =
                    LineChart.Axis.Ticks.custom
                        (\range _ ->
                            LineChart.Axis.Values.int
                                (LineChart.Axis.Values.around 5)
                                { range | min = 0 }
                                |> List.map LineChart.Axis.Tick.int
                        )
                }
        , container =
            --LineChart.Container.spaced "line-chart" 40 40 40 80
            LineChart.Container.custom
                { attributesHtml =
                    [ Svg.Attributes.color "rgb(255, 255, 255)"

                    --, Html.Attributes.style "margin-top" "20px"
                    ]
                , attributesSvg = []
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
            LineChart.Junk.default
        , grid =
            LineChart.Grid.lines
                0.1
                LineChart.Colors.black
        , area =
            LineChart.Area.default
        , line =
            LineChart.Line.wider
                3.0
        , dots =
            LineChart.Dots.default
        }
        [ LineChart.line LineChart.Colors.rust LineChart.Dots.circle "Würfel" <| Dict.toList results ]
        |> html
