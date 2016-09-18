module Playfield exposing (..)

import Model exposing (Model)
import Svg exposing (Svg, svg, rect, polygon, circle, g, defs, Attribute)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick, on)
import Msg exposing (Msg(SelectPiece, ClearPieceSelection, Mdl))
import Math.Vector2 as V2 exposing (Vec2, vec2, getX, getY, add, scale)
import Points
import Array
import Spaces exposing (Spaces, Space, SpaceType(..), SpaceIndex)
import Pieces exposing (Piece, PieceType(..), PieceControllability(..))
import Dict exposing (Dict)
import String


getPieces model =
    Dict.map (getPieceView model) model.pieces
        |> Dict.values


getSpaces model =
    Dict.toList model.spaces
        |> List.map
            (\spacePair ->
                getSpaceView model.showSpaceOutlines
                    (getPieceSelectedInfo model (fst spacePair))
                    spacePair
            )


getPieceSelectedInfo : Model -> SpaceIndex -> Maybe ( Int, Bool )
getPieceSelectedInfo model spaceIndex =
    Maybe.map
        (\index ->
            ( index, canPieceMoveToSpace model index spaceIndex )
        )
        model.pieceSelected


canPieceMoveToSpace : Model -> Int -> SpaceIndex -> Bool
canPieceMoveToSpace model index spaceIndex =
    let
        maybePiece =
            Dict.get index model.pieces
    in
        Maybe.map
            (\piece ->
                case piece.pieceType of
                    Star _ ->
                        isSpaceUnoccupied model spaceIndex

                    _ ->
                        Spaces.indexIsOfActualSpace model.spaces spaceIndex
            )
            maybePiece
            |> Maybe.withDefault False


isSpaceUnoccupied : Model -> SpaceIndex -> Bool
isSpaceUnoccupied model spaceIndex =
    Dict.get spaceIndex model.spaces
        |> Maybe.map
            (\space ->
                List.length (Pieces.getPiecesOnSpace model.pieces space.position) <= 0
            )
        |> Maybe.withDefault False


getSpaceView : Bool -> Maybe ( Int, Bool ) -> ( ( Int, Int ), Space ) -> Svg Msg
getSpaceView showOutlines pieceSelectedInfo ( index, currentSpace ) =
    let
        spaceType =
            currentSpace.spaceType

        extras =
            case pieceSelectedInfo of
                Nothing ->
                    [ stroke "grey" ]

                Just ( id, canMoveHere ) ->
                    let
                        baseExtras =
                            [ onClick <| Msg.MovePiece id index, cursor "pointer" ]
                    in
                        if canMoveHere then
                            baseExtras
                                ++ [ stroke "white" ]
                        else
                            baseExtras
                                ++ [ stroke "grey" ]

        finalExtras =
            if spaceType == EmptySpace then
                extras ++ [ pointerEvents "none" ]
            else
                extras
    in
        space showOutlines finalExtras (currentSpace.position) spaceType


getPieceView : Model -> Int -> Piece -> Svg Msg
getPieceView model currentID currentPiece =
    let
        selectedAttributes =
            getPieceAttributes model currentID currentPiece
    in
        piece selectedAttributes currentPiece.position currentPiece.pieceType


getPieceAttributes model currentID currentPiece =
    case model.pieceSelected of
        Just selectedID ->
            if selectedID == currentID then
                [ onClick ClearPieceSelection
                , stroke "white"
                , fillOpacity "0.5"
                ]
            else
                case Spaces.getSpaceFromPosition model.spaces currentPiece.position of
                    Just spaceIndex ->
                        [ onClick
                            <| Msg.MovePiece selectedID
                                spaceIndex
                        , cursor "pointer"
                        , stroke "grey"
                        ]

                    Nothing ->
                        [ cursor "not-allowed"
                        , stroke "grey"
                        ]

        Nothing ->
            [ onClick <| SelectPiece currentID, stroke "grey" ]


noPiece =
    polygon [] []


piece : List (Attribute Msg) -> Vec2 -> PieceType -> Svg Msg
piece extras center pieceType =
    let
        otherAttributes =
            basicPieceAttributes ++ extras
    in
        case pieceType of
            Star control ->
                polygonPiece
                    <| [ fill (getFill control)
                       , points (Points.star center)
                       ]
                    ++ otherAttributes

            WeirdThing control ->
                polygonPiece
                    <| [ fill (getFill control)
                       , points (Points.weirdThing center)
                       ]
                    ++ otherAttributes

            Triangle control ->
                polygonPiece
                    <| [ fill (getFill control)
                       , points (Points.triangle center)
                       ]
                    ++ otherAttributes

            Eye control ->
                eyePiece
                    (fill (getFill control)
                        :: otherAttributes
                    )
                    center

            NoPiece ->
                noPiece


basicPieceAttributes =
    [ strokeWidth "4"
    , cursor "move"
    ]


polygonPiece finalAttributes =
    polygon finalAttributes
        []


eyePiece : List (Attribute Msg) -> Vec2 -> Svg Msg
eyePiece attributes center =
    let
        centerX =
            getX center

        xString =
            centerX |> toString

        centerY =
            getY center

        yString =
            centerY |> toString

        {- graphical glitches can occur if these IDs aren't unique -}
        idString =
            "sclera" ++ xString ++ "_" ++ yString
    in
        g []
            [ Svg.defs []
                [ Svg.mask [ id idString ]
                    [ rect [ width "100%", height "100%", fill "white" ] []
                    , Svg.path
                        [ d <| eyePieceSclera centerX xString centerY yString
                        , fill "#000"
                        ]
                        []
                    ]
                ]
            , circle
                ([ cx xString
                 , cy yString
                 , r <| toString (Points.circleRadius / 2.25)
                 ]
                    ++ attributes
                )
                []
            , circle
                ([ cx xString
                 , cy yString
                 , r <| toString Points.circleRadius
                 , mask ("url(#" ++ idString ++ ")")
                 ]
                    ++ attributes
                )
                []
            ]


eyePieceSclera centerX xString centerY yString =
    let
        leftSideString =
            toString (centerX - Points.circleRadius)
                ++ " "
                ++ yString

        controlPointString =
            xString ++ " " ++ toString (centerY - Points.circleRadius)

        secondControlPointString =
            xString ++ " " ++ toString (centerY + Points.circleRadius)

        rightSideString =
            toString (centerX + Points.circleRadius) ++ " " ++ yString
    in
        String.join " "
            [ "M"
            , leftSideString
            , "Q"
            , controlPointString
            , rightSideString
            , "Q"
            , secondControlPointString
            , leftSideString
            ]


defs =
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
                    ]

                Red ->
                    [ fill "red"
                    ]

                Yellow ->
                    [ fill "#FFDC00"
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
            , strokeWidth "4"
            ]
                ++ appearance
                ++ extras
    in
        polygon attributes
            []
