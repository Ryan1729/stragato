module PiecesAndSpaces exposing (..)

import Spaces exposing (Spaces, SpaceType(..), SpaceIndex)
import Pieces exposing (Pieces, Piece, PieceType)
import Dict exposing (Dict)


getSelfMoves : List Int -> Pieces -> Spaces -> List ( Int, SpaceIndex )
getSelfMoves pieceList pieces spaces =
    List.filterMap
        (\index ->
            let
                maybePiece =
                    Dict.get index pieces

                maybeSpaceIndex =
                    Maybe.andThen maybePiece
                        (\piece ->
                            Spaces.getSpaceFromPosition spaces
                                piece.position
                        )
            in
                Maybe.map (\spaceIndex -> ( index, spaceIndex ))
                    maybeSpaceIndex
        )
        pieceList


getNonSelfSpaceIdicies : Pieces -> Spaces -> Int -> List SpaceIndex
getNonSelfSpaceIdicies pieces spaces index =
    Dict.keys spaces
        |> List.filter (pieceIsNotAtSpace pieces spaces index)


isSpaceOccupied : Pieces -> Spaces -> SpaceIndex -> Maybe Bool
isSpaceOccupied pieces spaces spaceIndex =
    spaces
        |> Spaces.getActualSpaces
        |> Dict.get spaceIndex
        |> Maybe.map
            (\space ->
                not <| Pieces.noPiecesAtPosition pieces space.position
            )


pieceIsNotAtSpace : Pieces -> Spaces -> Int -> SpaceIndex -> Bool
pieceIsNotAtSpace pieces spaces index spaceIndex =
    case ( Dict.get index pieces, Dict.get spaceIndex spaces ) of
        ( Just piece, Just space ) ->
            piece.position /= space.position

        _ ->
            True
