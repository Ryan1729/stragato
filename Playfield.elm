module Playfield exposing (..)

import GameModel exposing (Model)
import Svg exposing (Svg, svg, rect, polygon, circle, g, Attribute)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick, on)
import GameMsg exposing (Msg(SelectPiece, ClearPieceSelection))
import Math.Vector2 as V2 exposing (Vec2, vec2, getX, getY, add, scale)
import Points
import Array
import Spaces exposing (Spaces, Space, SpaceType(..), SpaceIndex)
import Pieces exposing (Piece, PieceType, Controller(..), MoveOccupancy(..), Shape(..))
import PiecesAndSpaces
import Dict exposing (Dict)
import String
import Movement
import PieceAppearances exposing (PieceAppearances)


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
            , Movement.canPieceMoveToSpace model.pieces
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
                            [ onClick <| GameMsg.MovePiece id index, cursor "pointer" ]
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
        piece model.exportModel.pieceAppearances selectedAttributes currentPiece.position currentPiece.pieceType


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
                            <| GameMsg.MovePiece selectedID
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
    GameModel.canMove model
        && (model.allowMovingAllPieces
                || Pieces.isPlayerControllable currentPiece
           )


piece : PieceAppearances -> List (Attribute msg) -> Vec2 -> PieceType -> Svg msg
piece pieceAppearances extras center pieceType =
    let
        otherAttributes =
            basicPieceAttributes ++ extras
    in
        case PieceAppearances.get pieceType pieceAppearances of
            ( PointsList pointsList, fillString ) ->
                polygonPiece
                    <| [ fill fillString
                       , points (Points.piecePointsListToSVGString pointsList center)
                       ]
                    ++ otherAttributes

            ( Eye, fillString ) ->
                eyePiece
                    (fill fillString
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


eyePiece : List (Attribute msg) -> Vec2 -> Svg msg
eyePiece attributes center =
    eyePieceSized Points.circleRadius attributes center


eyePieceSized : Float -> List (Attribute msg) -> Vec2 -> Svg msg
eyePieceSized radius attributes center =
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
                 , r <| toString (radius / 2.25)
                 ]
                    ++ attributes
                )
                []
            , circle
                ([ cx xString
                 , cy yString
                 , r <| toString radius
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


space : Bool -> List (Attribute msg) -> Vec2 -> SpaceType -> Svg msg
space showOutlines extras center spaceType =
    let
        appearance =
            case spaceType of
                Green ->
                    [ fill "#7FFF7f"
                    ]

                Red ->
                    [ fill "#FF9f7f"
                    ]

                Yellow ->
                    [ fill "#FFDC7f"
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
