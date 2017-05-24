{-
   A piece of "abstract art" composed in Elm using SVG and module Geometry.
   Run in Elm-reactor/
-}


module Main exposing (..)

import Svg as S exposing (..)
import Svg.Attributes as SA exposing (..)
import Graph as G


main =
    S.svg
        [ SA.width "1200", SA.height "1200" ]
        [ (G.boundingRect graphData)
        , square
        , redCircle2
        , redCircle3
        , rectangle5
        , rectangle4
        , rectangle3
        , circle5
        , circle6
        , blueCircle
        , circle4
        , greenCircle
        , yellowEllipse
        , horizontalRectangle
        , verticalRectangle
        , verticalRectangle2
        ]


source =
    G.Rect 0.0 0.0 200.0 200.0


target =
    G.Rect 60.0 60.0 500.0 500.0


graphData =
    G.GraphData source target "#ffffff" "black"


drawPoly =
    G.drawPolygon graphData


paintPoly =
    drawPoly "none"


drawRect =
    G.drawRect graphData


paintRect =
    drawRect "none"


drawEllipse =
    G.drawEllipse graphData


paintEllipse =
    G.drawEllipse graphData "none"


drawCircle =
    G.drawCircle graphData


paintCircle =
    G.drawCircle graphData "none"


square =
    paintRect "rgb(160,160,255)" 0.8 0.0 0.0 200.0 200.0


blueCircle =
    paintCircle "#5050FF" 0.7 141.1 59.0 59.0


greenCircle =
    paintCircle "#00FF00" 0.6 30.0 40.0 40.0


yellowEllipse =
    paintEllipse "rgb(240,240,10)" 0.6 60.0 120.0 43.0 75.0


redCircle2 =
    paintCircle "rgb(240,0,10)" 0.8 60.0 160.0 25.0


redCircle3 =
    paintCircle "rgb(240,0,10)" 1.0 80.0 120.0 13.0


circle4 =
    paintCircle "rgb(40,0,240)" 1.0 125.0 90.0 18.0


circle5 =
    paintCircle "rgb(40,0,240)" 1.0 170.0 34.0 9.0


circle6 =
    paintCircle "rgb(0,0,0)" 1.0 140.0 130.0 6.0


horizontalRectangle =
    G.drawRect graphData "black" "rgb(240,0,10)" 0.4 10 10 180 15


verticalRectangle =
    G.drawRect graphData "black" "rgb(240,0,10)" 0.3 40 4 10 190


verticalRectangle2 =
    G.drawRect graphData "black" "rgb(240,0,10)" 0.2 15 4 10 160


rectangle3 =
    G.drawRect graphData "none" "rgb(0,120,240)" 0.3 120 25 60 150


rectangle4 =
    G.drawRect graphData "none" "rgb(150,120,240)" 0.7 135 10 50 185


rectangle5 =
    G.drawRect graphData "none" "rgb(140,100,255)" 0.7 155 5 40 180
