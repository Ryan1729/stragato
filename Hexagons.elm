module Hexagons exposing (..)

import Math.Vector2 as V2 exposing (Vec2, vec2, getX, getY, add, scale)
import Extras


hexagonHeightConstant =
    (sqrt 3) / 2


grid : Int -> Int -> List ( ( Int, Int ), Vec2 )
grid width height =
    let
        baseList =
            [0..width * height - 1]
    in
        List.filterMap
            (\index ->
                let
                    x =
                        index % width

                    y =
                        index // width
                in
                    if (x + y) % 2 == 0 then
                        let
                            vector =
                                vec2 (toFloat x * 1.5)
                                    (toFloat y * hexagonHeightConstant)
                        in
                            Just ( ( x, y ), vector )
                    else
                        Nothing
            )
            baseList


ring : Int -> List ( Int, Int )
ring radius =
    if radius <= 0 then
        [ ( 0, 0 ) ]
    else if radius == 1 then
        [ ( 0, 2 )
        , ( 1, -1 )
        , ( 1, 1 )
        , ( 2, 0 )
        , ( -1, 1 )
        , ( -1, -1 )
        ]
    else
        let
            range =
                [1..radius]
        in
            List.scanl (Extras.ignoreFirstArg southEast) ( 0, radius * 2 ) range
                ++ List.scanl (Extras.ignoreFirstArg south) ( radius, -radius ) range
                ++ List.scanl (Extras.ignoreFirstArg southWest) ( radius, radius ) range
                ++ List.scanl (Extras.ignoreFirstArg northWest) ( radius * 2, 0 ) range
                ++ List.scanl (Extras.ignoreFirstArg north) ( -radius, radius ) range
                ++ List.scanl (Extras.ignoreFirstArg northEast) ( -radius, -radius ) range


southEast : ( Int, Int ) -> ( Int, Int )
southEast ( x, y ) =
    ( x + 1, y + 1 )


south : ( Int, Int ) -> ( Int, Int )
south ( x, y ) =
    ( x, y + 2 )


southWest : ( Int, Int ) -> ( Int, Int )
southWest ( x, y ) =
    ( x + 1, y + 1 )


northWest : ( Int, Int ) -> ( Int, Int )
northWest ( x, y ) =
    ( x + 1, y - 1 )


north : ( Int, Int ) -> ( Int, Int )
north ( x, y ) =
    ( x, y - 2 )


northEast : ( Int, Int ) -> ( Int, Int )
northEast ( x, y ) =
    ( x + 1, y - 1 )
