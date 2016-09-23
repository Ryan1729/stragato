module Points exposing (..)

import Math.Vector2 as V2 exposing (Vec2, vec2, getX, getY, add, scale)
import String


hexagonHeightConstant =
    (sqrt 3) / 2


hexGrid : Int -> Int -> List ( ( Int, Int ), Vec2 )
hexGrid width height =
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


tau =
    2 * pi


fractionToPointOnCircle : Float -> Vec2
fractionToPointOnCircle fraction =
    let
        angle =
            tau * fraction
    in
        vec2 (cos angle) (sin angle)


hexagonPointsList : List Vec2
hexagonPointsList =
    List.map fractionToPointOnCircle
        [ 0
        , 1 / 6
        , 2 / 6
        , 3 / 6
        , 4 / 6
        , 5 / 6
        ]


weirdThingPointsList : List Vec2
weirdThingPointsList =
    List.map fractionToPointOnCircle
        [ 0
        , 1 / 6
        , 5 / 6
        , 2 / 6
        , 4 / 6
        , 3 / 6
        ]



--I couldn't find a ready-made version of this!


fmod : Float -> Float -> Float
fmod value modulus =
    value - ((toFloat <| truncate (value / modulus)) * modulus)


petalsPointsList : List Vec2
petalsPointsList =
    [0..12]
        |> List.map (\x -> fmod (x * 4 / 13) 1)
        |> List.map fractionToPointOnCircle


twistedPlusPointsList : List Vec2
twistedPlusPointsList =
    List.map fractionToPointOnCircle
        [ 1 / 24
        , 17 / 24
        , 19 / 24
        , 19 / 24
        , 11 / 24
        , 13 / 24
        , 5 / 24
        , 7 / 24
        , 23 / 24
        ]


fangsPointsList : List Vec2
fangsPointsList =
    List.map fractionToPointOnCircle
        [ 3 / 24
        , 1 / 24
        , 23 / 24
        , 21 / 24
        , 5 / 24
        , 7 / 24
        , 15 / 24
        , 13 / 24
        , 11 / 24
        , 9 / 24
        , 17 / 24
        , 19 / 24
        ]


pointsListToPiecePointsList : List Vec2 -> List Vec2
pointsListToPiecePointsList =
    List.map (V2.scale 40)


spaceScale =
    60


circleRadius =
    spaceScale * 0.625


spacePointsList : List Vec2
spacePointsList =
    List.map (V2.scale spaceScale)
        hexagonPointsList


spaceWidth =
    spaceScale * 2


starPointsList : List Vec2
starPointsList =
    List.map ((+) (-1 / 4) >> fractionToPointOnCircle)
        [ 0
        , 2 / 5
        , 4 / 5
        , 1 / 5
        , 3 / 5
        ]


trianglePointsList : List Vec2
trianglePointsList =
    List.map ((+) (-1 / 4) >> fractionToPointOnCircle)
        [ 0
        , 2 / 5
        , 3 / 5
        ]


v2ToSVGString : Vec2 -> String
v2ToSVGString vector =
    let
        x =
            toString (getX vector)

        y =
            toString (getY vector)
    in
        x ++ "," ++ y


piecePointsListToSVGString : List Vec2 -> Vec2 -> String
piecePointsListToSVGString pointsList center =
    pointsList
        |> List.map (add center >> v2ToSVGString)
        |> String.join " "


pointsListToSVGString : List Vec2 -> Vec2 -> String
pointsListToSVGString =
    pointsListToPiecePointsList >> piecePointsListToSVGString


space : Vec2 -> String
space =
    piecePointsListToSVGString spacePointsList


star : Vec2 -> String
star =
    pointsListToSVGString starPointsList


weirdThing : Vec2 -> String
weirdThing =
    pointsListToSVGString weirdThingPointsList


triangle : Vec2 -> String
triangle =
    pointsListToSVGString trianglePointsList


petals : Vec2 -> String
petals =
    pointsListToSVGString petalsPointsList


twistedPlus : Vec2 -> String
twistedPlus =
    pointsListToSVGString twistedPlusPointsList


fangs : Vec2 -> String
fangs =
    pointsListToSVGString fangsPointsList
