module View exposing (..)

import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Math.Vector2 as V2 exposing (Vec2, vec2, getX, getY, add)
import String


view : () -> Html msg
view _ =
    svg [ width "600", height "600", viewBox "0 0 1024 1024" ]
        [ rect
            [ x "0"
            , y "0"
            , width "100%"
            , height "100%"
            , fill "#08f"
            ]
            []
        , polygon
            [ fill "lime"
            , points <| hexagonPoints (vec2 400 400) 60
            ]
            []
        ]


sin60 =
    sin (pi / 3)



{- derived from tables at http://www.rdwarf.com/lerickson/hex/ -}


hexagonPointsList =
    [ vec2 0 1.5
    , vec2 0 0.5
    , vec2 sin60 0
    , vec2 (2 * sin60) 0.5
    , vec2 (2 * sin60) 1.5
    , vec2 sin60 2
    ]


v2ToSVGString : Vec2 -> String
v2ToSVGString vector =
    (toString <| getX vector) ++ "," ++ (toString <| getY vector)


hexagonPoints : Vec2 -> Float -> String
hexagonPoints corner sideLength =
    hexagonPointsList
        |> List.map (V2.scale sideLength >> add corner >> v2ToSVGString)
        |> String.join " "
