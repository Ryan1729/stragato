module Movement exposing (..)

import Spaces exposing (Spaces, SpaceType(..), SpaceIndex)
import Pieces exposing (Pieces, Piece, PieceType, Shape(..), MoveType(..))
import PiecesAndSpaces
import Model exposing (Model)
import Extras
import Dict
import Deck
import Math.Vector2 as V2 exposing (Vec2, vec2)


getNewPieces : Model -> Int -> SpaceIndex -> Pieces
getNewPieces model pieceID spaceID =
    if
        canPieceMoveToSpace model.allowSelfMoves
            model.pieces
            model.spaces
            pieceID
            spaceID
    then
        movePieceToSpace model.pieces model.spaces pieceID spaceID
    else
        model.pieces


movePieceToSpace : Pieces -> Spaces -> Int -> SpaceIndex -> Pieces
movePieceToSpace pieces spaces index spaceIndex =
    case
        ( Dict.get index pieces
        , Spaces.getPosition spaceIndex spaces
        )
    of
        ( Just piece, Just targetSpacePosition ) ->
            case
                ( piece.pieceType.shape
                , piece.pieceType.controller
                , Pieces.getPiecesAtPosition pieces targetSpacePosition
                )
            of
                {- They have a fight, Triangle wins. Triangle man! -}
                ( Triangle, _, piecesOnSpace ) ->
                    pieces
                        |> Extras.filterOutListFromDict piecesOnSpace
                        |> Pieces.setPieceLocation index targetSpacePosition

                ( WeirdThing, _, piecesOnSpace ) ->
                    pieces
                        |> bumpPiecesOnce spaces piece.position targetSpacePosition
                        |> Pieces.setPieceLocation index targetSpacePosition

                ( TwistedPlus, _, piecesOnSpace ) ->
                    pieces
                        |> bumpPiecesNTimes 2 spaces piece.position targetSpacePosition
                        |> Pieces.setPieceLocation index targetSpacePosition

                ( Eye, _, piecesOnSpace ) ->
                    pieces
                        |> Pieces.movePieces targetSpacePosition piece.position
                        |> Pieces.setPieceLocation index targetSpacePosition

                ( Petals, control, piecesOnSpace ) ->
                    if Pieces.noPiecesAtPosition pieces targetSpacePosition then
                        pieces
                            |> Pieces.setPieceLocation index targetSpacePosition
                            |> Pieces.addPiece (Piece (PieceType Pieces.Petals control Unoccupied) piece.position)
                    else
                        pieces

                _ ->
                    if Pieces.noPiecesAtPosition pieces targetSpacePosition then
                        Pieces.setPieceLocation index targetSpacePosition pieces
                    else
                        pieces

        _ ->
            pieces


getPossibleMoveList : Model -> List ( Int, SpaceIndex )
getPossibleMoveList model =
    let
        cpuMovablePieces =
            Pieces.getCPUMovablePieces model.pieces

        actualSpaceIndicies =
            model.spaces
                |> Spaces.getActualSpaces
                |> Dict.keys

        allConcievableMoves =
            cpuMovablePieces
                `Extras.andThen` \x ->
                                    actualSpaceIndicies
                                        `Extras.andThen` \y ->
                                                            [ ( x, y ) ]
    in
        List.filter
            (\( index, spaceIndex ) ->
                canPieceMoveToSpace model.allowSelfMoves
                    model.pieces
                    model.spaces
                    index
                    spaceIndex
            )
            allConcievableMoves


canPieceMoveToSpace : Bool -> Pieces -> Spaces -> Int -> SpaceIndex -> Bool
canPieceMoveToSpace allowSelfMoves pieces spaces index spaceIndex =
    if allowSelfMoves || PiecesAndSpaces.pieceIsNotAtSpace pieces spaces index spaceIndex then
        let
            maybePiece =
                Dict.get index pieces
        in
            Maybe.map
                (\piece ->
                    case piece.pieceType.moveType of
                        Occupied ->
                            PiecesAndSpaces.isSpaceOccupied pieces spaces spaceIndex

                        Unoccupied ->
                            PiecesAndSpaces.isSpaceUnoccupied pieces spaces spaceIndex

                        AnySpace ->
                            Spaces.indexIsOfActualSpace spaces spaceIndex
                )
                maybePiece
                |> Maybe.withDefault False
    else
        False


bumpPiecesNTimes : Int -> Spaces -> Vec2 -> Vec2 -> Pieces -> Pieces
bumpPiecesNTimes n spaces position targetSpacePosition pieces =
    if n <= 0 then
        pieces
    else
        let
            bumpOnce =
                bumpPiecesOnce spaces position targetSpacePosition
        in
            case getBumpTargetSpacePosition spaces position targetSpacePosition of
                Just newTargetPosition ->
                    bumpOnce
                        (bumpPiecesNTimes (n - 1)
                            spaces
                            targetSpacePosition
                            newTargetPosition
                            pieces
                        )

                Nothing ->
                    bumpOnce pieces


bumpPiecesOnce : Spaces -> Vec2 -> Vec2 -> Pieces -> Pieces
bumpPiecesOnce spaces piecePosition spacePosition pieces =
    case getBumpTargetSpacePosition spaces piecePosition spacePosition of
        Just targetSpacePosition ->
            if Spaces.positionIsOnActualSpace spaces targetSpacePosition then
                Pieces.movePieces spacePosition targetSpacePosition pieces
            else
                Pieces.removePiecesAtPosition spacePosition pieces

        Nothing ->
            Pieces.removePiecesAtPosition spacePosition pieces


getBumpTargetSpacePosition : Spaces -> Vec2 -> Vec2 -> Maybe Vec2
getBumpTargetSpacePosition spaces piecePosition spacePosition =
    let
        maybeBumpingSpaceID =
            Spaces.getSpaceFromPosition spaces piecePosition

        maybeBumpedSpaceID =
            Spaces.getSpaceFromPosition spaces spacePosition
    in
        Maybe.map2 getBumpTargetID maybeBumpingSpaceID maybeBumpedSpaceID
            `Maybe.andThen` (\targetID -> Dict.get targetID spaces)
            |> Maybe.map .position


getBumpTargetID ( bumpingX, bumpingY ) ( bumpedX, bumpedY ) =
    ( bumpedX + (bumpedX - bumpingX), bumpedY + (bumpedY - bumpingY) )
