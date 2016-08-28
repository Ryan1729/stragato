module View exposing (..)

import Html exposing (Html)
import Svg exposing (Svg, svg, rect, polygon, Attribute)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick, on)
import Math.Vector2 as V2 exposing (Vec2, vec2, getX, getY, add, scale)
import Msg exposing (Msg(PlayClack, SelectPiece))
import Model exposing (Model, Piece, PieceType(..))
import Mouse
import Json.Decode
import Points


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


view : Model -> Html Msg
view model =
    let
        selectedId =
            case model.pieceSelected of
                Nothing ->
                    -1

                Just id ->
                    id
    in
        svg [ width "600", height "600", viewBox "0 0 600 600" ]
            <| background
            ++ (List.map (space << add (vec2 100 100) << V2.scale 60)
                    <| hexGridPoints model.width model.height
               )
            ++ List.indexedMap (getPieceView selectedId) model.pieceList


getPieceView : Int -> Int -> Piece -> Svg Msg
getPieceView selectedId currentId piece =
    let
        isSelected =
            selectedId == currentId
    in
        case piece.pieceType of
            Star ->
                starPiece piece.position isSelected currentId

            WeirdThing ->
                weirdThingPiece piece.position isSelected currentId


hexagonHeightConstant =
    (sqrt 3) / 2


hexGridPoints : Int -> Int -> List Vec2
hexGridPoints width height =
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
                        Just
                            <| vec2 (toFloat x * 1.5)
                                (toFloat y * hexagonHeightConstant)
                    else
                        Nothing
            )
            baseList


starPiece : Vec2 -> Bool -> Int -> Svg Msg
starPiece center =
    piece <| Points.star center


weirdThingPiece : Vec2 -> Bool -> Int -> Svg Msg
weirdThingPiece center =
    piece <| Points.weirdThing center


piece : String -> Bool -> Int -> Svg Msg
piece piecesPoints selected id =
    polygon
        [ fill "#fa0"
        , points piecesPoints
        , stroke "grey"
        , strokeWidth "4"
        , cursor "move"
        , onClick <| SelectPiece id
        , fillOpacity
            <| if selected then
                "0.5"
               else
                "1.0"
        ]
        []


space center =
    polygon
        [ fill "lime"
        , points <| Points.space center
        , stroke "grey"
        , strokeWidth "4"
        , onClick <| Msg.SpaceClicked 1
        ]
        []
