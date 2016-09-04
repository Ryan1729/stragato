module Playfield exposing (..)

import Model exposing (Model)
import Svg exposing (Svg, svg, rect, polygon, Attribute)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick, on)
import Msg exposing (Msg(SelectPiece, ClearPieceSelection, Mdl))
import Math.Vector2 as V2 exposing (Vec2, vec2, getX, getY, add, scale)
import Points
import Array
import PlayfieldComponents exposing (Piece, PieceType(..), Spaces, SpaceType(..))


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

        finalExtras =
            if spaceType == EmptySpace then
                extras ++ [ pointerEvents "none" ]
            else
                extras
    in
        space finalExtras center spaceType


getPieceView : Int -> Int -> Piece -> Svg Msg
getPieceView selectedId currentId currentPiece =
    let
        isSelected =
            selectedId == currentId

        selectedAttributes =
            if isSelected then
                [ onClick ClearPieceSelection
                , fillOpacity "0.5"
                ]
            else
                [ onClick <| SelectPiece currentId ]
    in
        piece selectedAttributes currentPiece.position currentPiece.pieceType


piece : List (Attribute Msg) -> Vec2 -> PieceType -> Svg Msg
piece extras center pieceType =
    let
        piecesPoints =
            case pieceType of
                Star ->
                    Points.star center

                WeirdThing ->
                    Points.weirdThing center

                NoPiece ->
                    ""

        attributes =
            [ fill "#fa0"
            , points piecesPoints
            , stroke "grey"
            , strokeWidth "4"
            , cursor "move"
            ]

        finalAttributes =
            attributes ++ extras
    in
        polygon finalAttributes
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
                    [ fillOpacity "0.0"
                    , strokeOpacity "0.0"
                    ]

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
