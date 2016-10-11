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
import Pieces exposing (Piece, PieceType, Controller(..), MoveType(..), Shape(..))
import PiecesAndSpaces
import Dict exposing (Dict)
import String
import Movement
import PieceAppearances exposing (PieceAppearances, Icon(..))


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
        case ( PieceAppearances.get pieceType pieceAppearances, pieceType.moveType ) of
            ( ( PointsList pointsList, fillString, icon ), moveType ) ->
                (polygonPiece
                    <| [ fill fillString
                       , points (Points.piecePointsListToSVGString pointsList center)
                       ]
                    ++ otherAttributes
                )
                    |> addIcon icon center

            ( ( Eye, fillString, icon ), moveType ) ->
                eyePiece
                    (fill fillString
                        :: otherAttributes
                    )
                    center
                    |> addIcon icon center


addIcon : Icon -> Vec2 -> Svg msg -> Svg msg
addIcon icon center pieceView =
    let
        baseList =
            case icon of
                NoIcon ->
                    []

                EmptySpaceIcon ->
                    [ emptySpaceIcon center ]

                ShapeSpaceIcon shape ->
                    [ makeShapeIcon center shape, emptySpaceIcon center ]

                ShapeIcon shape ->
                    [ makeShapeIcon center shape, emptySpaceIcon center ]
    in
        g []
            <| pieceView
            :: baseList


iconOffset =
    vec2 Points.pieceScaleFactor Points.pieceScaleFactor
        |> V2.scale (0.6)


iconScale =
    0.25


pieceScale =
    iconScale * (Points.pieceScaleFactor / Points.spaceScale)


scaledSpacePointsList =
    List.map (V2.scale iconScale)
        Points.spacePointsList


emptySpaceIcon : Vec2 -> Svg msg
emptySpaceIcon center =
    let
        iconCenter =
            V2.add iconOffset
                center
    in
        polygon
            [ iconCenter
                |> Points.piecePointsListToSVGString scaledSpacePointsList
                |> points
            , strokeWidth "4"
            , fillOpacity "0.0"
            , stroke "black"
            ]
            []


makeShapeIcon center shape =
    let
        iconCenter =
            V2.add iconOffset center
    in
        case shape of
            PointsList pointsList ->
                polygonPiece
                    [ points
                        (Points.piecePointsListToSVGString (List.map (V2.scale pieceScale) pointsList)
                            iconCenter
                        )
                    , strokeWidth "4"
                    , fillOpacity "0.0"
                    , stroke "black"
                    ]

            Eye ->
                eyePieceSized (Points.circleRadius * pieceScale)
                    [ strokeWidth "4"
                    , fillOpacity "0.0"
                    , stroke "black"
                    ]
                    iconCenter


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
