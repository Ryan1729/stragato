module Playfield exposing (..)

import Model exposing (Model, Piece, PieceType(..), Spaces, SpaceType(..))
import Svg exposing (Svg, svg, rect, polygon, Attribute)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick, on)
import Msg exposing (Msg(SelectPiece, Mdl))
import Math.Vector2 as V2 exposing (Vec2, vec2, getX, getY, add, scale)
import Points
import Array


getPieces model =
    let
        selectedId =
            Maybe.withDefault -1 model.pieceSelected
    in
        Array.indexedMap (getPieceView selectedId) model.pieces
            |> Array.toList


getSpaces model =
    Array.indexedMap (getSpaceView model.pieceSelected model.spaces) model.spaces.positions
        |> Array.toList


getSpaceView : Maybe Int -> Spaces -> Int -> Vec2 -> Svg Msg
getSpaceView pieceSelected spaces index center =
    let
        spaceType =
            Array.get index spaces.types
                |> Maybe.withDefault EmptySpace

        extras =
            case pieceSelected of
                Nothing ->
                    []

                Just id ->
                    [ onClick <| Msg.MovePiece id index, cursor "pointer" ]
    in
        space extras center spaceType


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


space : List (Attribute Msg) -> Vec2 -> SpaceType -> Svg Msg
space extras center spaceType =
    let
        appearance =
            case spaceType of
                Green ->
                    [ fill "lime"
                    , stroke "grey"
                    , strokeWidth "4"
                    ]

                Red ->
                    [ fill "red"
                    , stroke "grey"
                    , strokeWidth "4"
                    ]

                EmptySpace ->
                    [ fillOpacity "0.0" ]

        attributes =
            [ points <| Points.space center
            , stroke "grey"
            , strokeWidth "4"
            ]
                ++ appearance
                ++ extras
    in
        polygon attributes
            []
