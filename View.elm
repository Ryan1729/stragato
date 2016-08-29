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
import Array


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
    svg [ width "600", height "600", viewBox "0 0 600 600" ]
        <| background
        ++ getSpaces model
        ++ getPieces model


getPieces model =
    let
        selectedId =
            Maybe.withDefault -1 model.pieceSelected
    in
        List.indexedMap (getPieceView selectedId) model.pieceList


getSpaces model =
    Array.indexedMap (getSpaceView model.pieceSelected) model.spaces.positions
        |> Array.toList


getSpaceView : Maybe Int -> Int -> Vec2 -> Svg Msg
getSpaceView pieceSelected index center =
    let
        extras =
            case pieceSelected of
                Nothing ->
                    []

                Just id ->
                    [ onClick <| Msg.MovePiece id index, cursor "pointer" ]
    in
        space extras center


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


starPiece : Vec2 -> Bool -> Int -> Svg Msg
starPiece center =
    piece <| Points.star center


weirdThingPiece : Vec2 -> Bool -> Int -> Svg Msg
weirdThingPiece center =
    piece <| Points.weirdThing center


piece : String -> Bool -> Int -> Svg Msg
piece piecesPoints selected id =
    let
        attributes =
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
    in
        polygon attributes
            []


space : List (Attribute Msg) -> Vec2 -> Svg Msg
space extras center =
    let
        attributes =
            [ fill "lime"
            , points <| Points.space center
            , stroke "grey"
            , strokeWidth "4"
            ]
                ++ extras
    in
        polygon attributes
            []
