module Playfield exposing (..)

import Model exposing (Model)
import Svg exposing (Svg, svg, rect, polygon, Attribute)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick, on)
import Msg exposing (Msg(SelectPiece, ClearPieceSelection, Mdl))
import Math.Vector2 as V2 exposing (Vec2, vec2, getX, getY, add, scale)
import Points
import Array
import PlayfieldComponents
    exposing
        ( Piece
        , PieceType(..)
        , PieceControllability(..)
        )
import Spaces exposing (Spaces, Space, SpaceType(..))
import Dict exposing (Dict)


getPieces model =
    let
        selectedId =
            Maybe.withDefault -1 model.pieceSelected
    in
        Dict.map (getPieceView selectedId) model.pieces
            |> Dict.values


getSpaces model =
    Dict.toList model.spaces
        |> List.map (getSpaceView model.showSpaceOutlines model.pieceSelected)


getSpaceView : Bool -> Maybe Int -> ( ( Int, Int ), Space ) -> Svg Msg
getSpaceView showOutlines pieceSelected ( index, currentSpace ) =
    let
        spaceType =
            currentSpace.spaceType

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
        space showOutlines finalExtras (currentSpace.position) spaceType


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
        ( piecesPoints, pieceFill ) =
            case pieceType of
                Star control ->
                    ( Points.star center, getFill control )

                WeirdThing control ->
                    ( Points.weirdThing center, getFill control )

                Triangle control ->
                    ( Points.triangle center, getFill control )

                NoPiece ->
                    ( "", "" )

        attributes =
            [ fill pieceFill
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


getFill control =
    case control of
        Player ->
            "#fa0"

        Computer ->
            "#0af"

        Both ->
            "#faf"

        None ->
            "#0a0"


space : Bool -> List (Attribute Msg) -> Vec2 -> SpaceType -> Svg Msg
space showOutlines extras center spaceType =
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

                Yellow ->
                    [ fill "#FFDC00"
                    , stroke "grey"
                    , strokeWidth "4"
                    ]

                EmptySpace ->
                    [ fillOpacity "0.0" ]
                        ++ if showOutlines then
                            []
                           else
                            [ strokeOpacity "0.0"
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
