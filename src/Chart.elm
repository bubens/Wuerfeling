module Chart exposing (render)

import Array exposing (Array)
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
import LineChart.Colors
import LineChart.Container
import LineChart.Dots
import LineChart.Events
import LineChart.Grid
import LineChart.Interpolation
import LineChart.Junk
import LineChart.Legends
import LineChart.Line


type alias Result =
    { count : Int
    , sum : Int
    }


render : Array Result -> Element msg
render results =
    LineChart.viewCustom
        { x =
            LineChart.Axis.custom
                { title = LineChart.Axis.Title.default "Augen"
                , variable = Just << toFloat << (+) 1 << .sum
                , pixels = 800
                , range = LineChart.Axis.Range.default
                , axisLine = LineChart.Axis.Line.default
                , ticks = LineChart.Axis.Ticks.int <| Array.length results
                }
        , y =
            LineChart.Axis.custom
                { title = LineChart.Axis.Title.default "Anzahl"
                , variable = Just << toFloat << .count
                , pixels = 300
                , range =
                    LineChart.Axis.Range.custom
                        (\{ min, max } ->
                            { min = 0
                            , max = max + max / 10
                            }
                        )
                , axisLine = LineChart.Axis.Line.default
                , ticks =
                    LineChart.Axis.Ticks.intCustom
                        10
                        (\x ->
                            LineChart.Axis.Tick.custom
                                { position = toFloat x
                                , color = LineChart.Colors.black
                                , width = 2
                                , length = 10
                                , grid = True
                                , direction = LineChart.Axis.Tick.negative
                                , label = Just (LineChart.Junk.label LineChart.Colors.black (String.fromInt x))
                                }
                        )
                }
        , container = LineChart.Container.default "line-chart-1"
        , interpolation = LineChart.Interpolation.default
        , intersection = LineChart.Axis.Intersection.default
        , legends = LineChart.Legends.none
        , events = LineChart.Events.default
        , junk = LineChart.Junk.default
        , grid = LineChart.Grid.default
        , area = LineChart.Area.default
        , line = LineChart.Line.default
        , dots = LineChart.Dots.default
        }
        [ LineChart.line LineChart.Colors.pink LineChart.Dots.circle "Würfel" <| Array.toList results ]
        |> html
