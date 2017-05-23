module Graph exposing (render, prepareIntegerSeries, flipSeries, scaleSeries)

import Svg exposing (..)
import Svg.Attributes as SA exposing (..)


type alias Point =
    ( Float, Float )


type alias Points =
    List Point


type alias GraphData =
    { xmin : Float
    , xmax : Float
    , ymin : Float
    , ymax : Float
    , points : Points
    }



-- There is a function in the List library called map2 that
-- applies a function pairwise to two lists. You can use it
-- to define 'zip' much more easily:
--


zip : List a -> List b -> List ( a, b )
zip =
    List.map2 (,)


timeSeries : List Float -> List ( Float, Float )
timeSeries data =
    let
        n =
            List.length data

        timeSequence =
            List.range 0 (n - 1) |> List.map toFloat
    in
        zip timeSequence data


scaleSeries : Float -> Float -> Points -> Points
scaleSeries kx ky points =
    List.map (\p -> ( kx * (Tuple.first p), ky * (Tuple.second p) )) points


flipSeries : Float -> Points -> Points
flipSeries a points =
    List.map (\p -> ( (Tuple.first p), a - (Tuple.second p) )) points


prepareIntegerSeries : List Int -> List ( Float, Float )
prepareIntegerSeries series =
    series |> List.map toFloat |> timeSeries



-- pair2String : ( a, b ) -> String


point2String : Point -> String
point2String point =
    (toString (Tuple.first point)) ++ ", " ++ (toString (Tuple.second point))



-- data2SVG [(0, 2), (1,4), (2,3)]
--   => "0,2  1,4  2,3"
-- data2SVG : GraphData -> Svg Msg


data2SVG : Points -> String
data2SVG data =
    List.map point2String data |> String.join " "



--
-- The (,) expression is a shortcut to create 2-tuples, so
-- evaluating ((,) 3 4) results in (3,4)
--     polyline [ fill "none", stroke "red", points (data2SVG data) ] []


render data =
    -- polyline [ fill "none", stroke "red", points (data2SVG data) ] []
    polyline [ fill "none", stroke "yellow", points (data2SVG data) ] []
