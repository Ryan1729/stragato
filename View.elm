module View exposing (..)

import Html exposing (Html, div)
import Html.Attributes as HA
import Html.Events as HE
import Svg exposing (Svg, svg, rect, polygon, Attribute)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick, on)
import Math.Vector2 as V2 exposing (Vec2, vec2, getX, getY, add, scale)
import Msg exposing (Msg(PlayClack, SelectPiece, GenerateBoard))
import Model exposing (Model, Piece, PieceType(..), Spaces, SpaceType(..))
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
    let
        playfield =
            [ svg [ width "600", height "400", viewBox "0 0 600 400" ]
                <| background
                ++ getSpaces model
                ++ getPieces model
            ]

        elements =
            if model.debug then
                playfield
                    ++ [ Html.hr [] []
                       , Html.button [ HE.onClick GenerateBoard ] [ Html.text "Generate Board" ]
                       , Html.text <| toString model
                       ]
            else
                playfield
    in
        div [ HA.style [ ( "color", "#DDD" ) ] ]
            elements


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
        space spaceType extras center


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


space : SpaceType -> List (Attribute Msg) -> Vec2 -> Svg Msg
space spaceType extras center =
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
