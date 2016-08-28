module Points exposing (space, star, weirdThing)

import Math.Vector2 as V2 exposing (Vec2, vec2, getX, getY, add, scale)
import String


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


weirdThingPiecePointsList : List Vec2
weirdThingPiecePointsList =
    List.map (V2.scale 40)
        weirdThingPointsList


spacePointsList : List Vec2
spacePointsList =
    List.map (V2.scale 60)
        hexagonPointsList


starPointsList : List Vec2
starPointsList =
    List.map ((+) (7 / 48) >> fractionToPointOnCircle)
        [ 0
        , 2 / 5
        , 4 / 5
        , 1 / 5
        , 3 / 5
        ]


starPiecePointsList : List Vec2
starPiecePointsList =
    List.map (V2.scale 40)
        starPointsList


v2ToSVGString : Vec2 -> String
v2ToSVGString vector =
    let
        x =
            toString (getX vector)

        y =
            toString (getY vector)
    in
        x ++ "," ++ y


pointsListToSVGString : List Vec2 -> Vec2 -> String
pointsListToSVGString pointsList center =
    pointsList
        |> List.map (add center >> v2ToSVGString)
        |> String.join " "


space : Vec2 -> String
space =
    pointsListToSVGString spacePointsList


star : Vec2 -> String
star =
    pointsListToSVGString starPiecePointsList


weirdThing : Vec2 -> String
weirdThing =
    pointsListToSVGString weirdThingPiecePointsList
