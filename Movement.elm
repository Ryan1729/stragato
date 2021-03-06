module Movement exposing (..)

import Spaces exposing (Spaces, SpaceType(..), SpaceIndex)
import Pieces exposing (Pieces, Piece, PieceType, Shape(..), MoveOccupancy(..), MoveEffect(..))
import PiecesAndSpaces
import Model exposing (Model)
import Extras
import Dict
import Deck
import Math.Vector2 as V2 exposing (Vec2, vec2)
import PieceAppearances exposing (PieceAppearances)
import PosInt
import ExportModel exposing (ExportModel)


getNewPieces : { model | spaces : Spaces, pieces : Pieces, exportModel : ExportModel } -> Int -> SpaceIndex -> Pieces
getNewPieces model pieceID spaceID =
    if
        canPieceMoveToSpace model.pieces
            model.spaces
            pieceID
            spaceID
    then
        movePieceToSpace model.exportModel.pieceAppearances model.pieces model.spaces pieceID spaceID
    else
        model.pieces


movePieceToSpace : PieceAppearances -> Pieces -> Spaces -> Int -> SpaceIndex -> Pieces
movePieceToSpace pieceAppearances pieces spaces index spaceIndex =
    case
        ( Dict.get index pieces
        , Spaces.getPosition spaceIndex spaces
        )
    of
        ( Just piece, Just targetSpacePosition ) ->
            case piece.pieceType.moveEffect of
                Capture ->
                    pieces
                        |> Extras.filterOutListFromDict (Pieces.getPiecesAtPosition pieces targetSpacePosition)
                        |> Pieces.setPieceLocation index targetSpacePosition

                Bump posInt ->
                    case PosInt.toInt posInt of
                        1 ->
                            pieces
                                |> bumpPiecesOnce spaces piece.position targetSpacePosition
                                |> Pieces.setPieceLocation index targetSpacePosition

                        n ->
                            pieces
                                |> bumpPiecesNTimes n spaces piece.position targetSpacePosition
                                |> Pieces.setPieceLocation index targetSpacePosition

                Swap ->
                    pieces
                        |> Pieces.movePieces targetSpacePosition piece.position
                        |> Pieces.setPieceLocation index targetSpacePosition

                Copy ->
                    if Pieces.noPiecesAtPosition pieces targetSpacePosition then
                        pieces
                            |> Pieces.setPieceLocation index targetSpacePosition
                            |> Pieces.addPiece piece
                    else
                        pieces

                NoEffect ->
                    if Pieces.noPiecesAtPosition pieces targetSpacePosition then
                        Pieces.setPieceLocation index targetSpacePosition pieces
                    else
                        pieces

        _ ->
            pieces


getPossibleMoveList : { model | spaces : Spaces, pieces : Pieces } -> List ( Int, SpaceIndex )
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
                canPieceMoveToSpace model.pieces
                    model.spaces
                    index
                    spaceIndex
            )
            allConcievableMoves


canPieceMoveToSpace : Pieces -> Spaces -> Int -> SpaceIndex -> Bool
canPieceMoveToSpace pieces spaces index spaceIndex =
    if PiecesAndSpaces.pieceIsNotAtSpace pieces spaces index spaceIndex then
        let
            maybePiece =
                Dict.get index pieces
        in
            maybePiece
                |> Maybe.map
                    (\piece ->
                        case PiecesAndSpaces.isSpaceOccupied pieces spaces spaceIndex of
                            Just bool ->
                                case Spaces.getIndexOffsetFromPosition spaces spaceIndex piece.position of
                                    Just pieceOffset ->
                                        let
                                            searchedList =
                                                if bool then
                                                    piece.pieceType.movePattern.occupied
                                                else
                                                    piece.pieceType.movePattern.unoccupied
                                        in
                                            List.any ((==) pieceOffset) searchedList

                                    Nothing ->
                                        False

                            Nothing ->
                                False
                    )
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
