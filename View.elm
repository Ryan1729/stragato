module View exposing (..)

import Html exposing (Html)
import Svg exposing (svg, rect, polygon, Attribute)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick, on)
import Math.Vector2 as V2 exposing (Vec2, vec2, getX, getY, add, scale, fromRecord)
import String
import Msg exposing (Msg(PlayClack, PieceDragStart))
import Model exposing (Model)
import Mouse
import Json.Decode


background =
    [ rect
        [ x "0"
        , y "0"
        , width "100%"
        , height "100%"
        , fill "#08f"
        , onClick PlayClack
        , cursor "pointer"
        ]
        []
    ]
        ++ List.map (space << add (vec2 400 400) << V2.scale 60)
            [ (vec2 0 0)
            , (vec2 1.5 hexagonHeightConstant)
            , (vec2 0 <| 2 * hexagonHeightConstant)
            , (vec2 1.5 <| 3 * hexagonHeightConstant)
            ]


view : Model -> Html Msg
view model =
    svg [ width "600", height "600", viewBox "0 0 600 600" ]
        <| background
        ++ [ piece model.piecePosition ]


piece center =
    polygon
        [ fill "#fa0"
        , points <| piecePoints center
        , stroke "grey"
        , strokeWidth "4"
        , onMouseDownWithPosition PieceDragStart
        , cursor "move"
        ]
        []


onMouseDownWithPosition : (Mouse.Position -> Msg) -> Attribute Msg
onMouseDownWithPosition msgConstructor =
    let
        msgDecoder =
            Json.Decode.map msgConstructor Mouse.position
    in
        on "mousedown" msgDecoder


space center =
    polygon
        [ fill "lime"
        , points <| spacePoints center
        , stroke "grey"
        , strokeWidth "4"
        ]
        []


hexagonHeightConstant =
    (sqrt 3) / 2


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


piecePointsList : List Vec2
piecePointsList =
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


spacePoints : Vec2 -> String
spacePoints =
    pointsListToSVGString spacePointsList


piecePoints : Vec2 -> String
piecePoints =
    pointsListToSVGString piecePointsList
