module Playfield exposing (..)

import Model exposing (Model)
import Svg exposing (Svg, svg, rect, polygon, circle, g, Attribute)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick, on)
import Msg exposing (Msg(SelectPiece, ClearPieceSelection, Mdl))
import Math.Vector2 as V2 exposing (Vec2, vec2, getX, getY, add, scale)
import Points
import Array
import Spaces exposing (Spaces, Space, SpaceType(..), SpaceIndex)
import Pieces exposing (Piece, PieceType, Controller(..), MoveType(..), Shape(..))
import PiecesAndSpaces
import Dict exposing (Dict)
import String
import Movement


getPieces model =
    Dict.map (getPieceView model) model.pieces
        |> Dict.values


getSpaces : Model -> List (Svg Msg)
getSpaces model =
    let
        baseSpaces =
            Dict.toList model.spaces
                |> List.map
                    (\spacePair ->
                        getSpaceView model.showSpaceOutlines
                            (getPieceSelectedInfo model (fst spacePair))
                            spacePair
                    )

        spaceHighlights =
            Dict.toList model.spaces
                |> List.map
                    (\spacePair ->
                        let
                            maybePieceSelectedInfo =
                                getPieceSelectedInfo model (fst spacePair)
                        in
                            getSpaceHighlightView model.showSpaceOutlines
                                (Maybe.map snd maybePieceSelectedInfo)
                                spacePair
                    )
    in
        baseSpaces ++ spaceHighlights


getPieceSelectedInfo : Model -> SpaceIndex -> Maybe ( Int, Bool )
getPieceSelectedInfo model spaceIndex =
    Maybe.map
        (\index ->
            ( index
            , Movement.canPieceMoveToSpace model.allowSelfMoves
                model.pieces
                model.spaces
                index
                spaceIndex
            )
        )
        model.pieceSelected


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


getSpaceHighlightView : Bool -> Maybe (Bool) -> ( ( Int, Int ), Space ) -> Svg Msg
getSpaceHighlightView showOutlines pieceSelectedInfo ( index, currentSpace ) =
    case pieceSelectedInfo of
        Nothing ->
            nullSVG

        Just canMoveHere ->
            if canMoveHere then
                space showOutlines
                    [ pointerEvents "none", stroke "white", strokeWidth "4" ]
                    (currentSpace.position)
                    currentSpace.spaceType
            else
                nullSVG


nullSVG =
    polygon [] []


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
            if shouldAllowSelecting model currentPiece then
                [ onClick <| SelectPiece currentID, stroke "grey" ]
            else
                [ cursor "not-allowed", stroke "grey" ]


shouldAllowSelecting : Model -> Piece -> Bool
shouldAllowSelecting model currentPiece =
    Model.canMove model
        && (model.allowMovingAllPieces
                || Pieces.isPlayerControllable currentPiece
           )


piece : List (Attribute Msg) -> Vec2 -> PieceType -> Svg Msg
piece extras center pieceType =
    let
        otherAttributes =
            basicPieceAttributes ++ extras
    in
        --TODO get record pattern matching working/report bug
        case ( pieceType.shape, pieceType.controller, pieceType.moveType ) of
            ( Star, control, moveType ) ->
                polygonPiece
                    <| [ fill (getFill control)
                       , points (Points.star center)
                       , transform (getTransform moveType)
                       ]
                    ++ otherAttributes

            ( WeirdThing, control, moveType ) ->
                polygonPiece
                    <| [ fill (getFill control)
                       , points (Points.weirdThing center)
                       , transform (getTransform moveType)
                       ]
                    ++ otherAttributes

            ( Triangle, control, moveType ) ->
                polygonPiece
                    <| [ fill (getFill control)
                       , points (Points.triangle center)
                       , transform (getTransform moveType)
                       ]
                    ++ otherAttributes

            ( Petals, control, moveType ) ->
                polygonPiece
                    <| [ fill (getFill control)
                       , points (Points.petals center)
                       , transform (getTransform moveType)
                       ]
                    ++ otherAttributes

            ( TwistedPlus, control, moveType ) ->
                polygonPiece
                    <| [ fill (getFill control)
                       , points (Points.twistedPlus center)
                       , transform (getTransform moveType)
                       ]
                    ++ otherAttributes

            ( Fangs, control, moveType ) ->
                polygonPiece
                    <| [ fill (getFill control)
                       , points (Points.fangs center)
                       , transform (getTransform moveType)
                       ]
                    ++ otherAttributes

            ( Eye, control, moveType ) ->
                eyePiece
                    (fill (getFill control)
                        :: otherAttributes
                    )
                    center


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


getTransform moveType =
    case moveType of
        Occupied ->
            "scale(1.1 1)"

        Unoccupied ->
            "scale(1 0.9)"

        AnySpace ->
            ""


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
