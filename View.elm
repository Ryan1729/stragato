module View exposing (..)

import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (width, height, viewBox, x, y, fill, stroke, strokeWidth, points)
import Svg.Events exposing (onClick)
import Math.Vector2 as V2 exposing (Vec2, vec2, getX, getY, add, scale)
import String
import Msg exposing (Msg(PlayClack))


view : () -> Html Msg
view _ =
    svg [ width "600", height "600", viewBox "0 0 1024 1024" ]
        <| [ rect
                [ x "0"
                , y "0"
                , width "100%"
                , height "100%"
                , fill "#08f"
                , onClick PlayClack
                ]
                []
           ]
        ++ List.map (hexagon << add (vec2 400 400) << scale 60)
            [ (vec2 0 0)
            , (vec2 1.5 hexagonHeightConstant)
            , (vec2 0 <| 2 * hexagonHeightConstant)
            , (vec2 1.5 <| 3 * hexagonHeightConstant)
            ]


hexagonHeightConstant =
    (sqrt 3) / 2


hexagon center =
    polygon
        [ fill "lime"
        , points <| hexagonPoints center 60
        , stroke "grey"
        , strokeWidth "4"
        ]
        []


tau =
    2 * pi


hexagonPointsList =
    List.map
        (\fraction ->
            let
                angle =
                    tau * fraction
            in
                vec2 (cos angle) (sin angle)
        )
        [ 0
        , 1 / 6
        , 2 / 6
        , 3 / 6
        , 4 / 6
        , 5 / 6
        ]


v2ToSVGString : Vec2 -> String
v2ToSVGString vector =
    (toString <| getX vector) ++ "," ++ (toString <| getY vector)


hexagonPoints : Vec2 -> Float -> String
hexagonPoints center sideLength =
    hexagonPointsList
        |> List.map (V2.scale sideLength >> add center >> v2ToSVGString)
        |> String.join " "
