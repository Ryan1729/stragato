module Pieces exposing (..)

import Math.Vector2 exposing (Vec2)
import Dict exposing (Dict)


type alias Piece =
    { pieceType : PieceType
    , position : Vec2
    }


type alias Pieces =
    Dict Int Piece


type PieceControllability
    = Player
    | Computer
    | Both
    | None


pieceControllabilityPossibilities =
    [ Player
    , Computer
    , Both
    , None
    ]


type PieceType
    = Star PieceControllability
    | WeirdThing PieceControllability
    | Triangle PieceControllability
    | Eye PieceControllability
    | NoPiece


controllablePossibilities =
    List.concatMap
        (\f ->
            List.concatMap
                (\x ->
                    [ f x ]
                )
                pieceControllabilityPossibilities
        )
        [ Star, WeirdThing, Triangle, Eye ]


pieceTypePossibilities =
    controllablePossibilities ++ [ NoPiece ]


isActualPiece : Piece -> Bool
isActualPiece piece =
    piece.pieceType /= NoPiece


setPieceLocation : Int -> Vec2 -> Pieces -> Pieces
setPieceLocation pieceId position pieces =
    Dict.update pieceId
        (Maybe.map
            (\piece ->
                { piece | position = position }
            )
        )
        pieces


getPiecesOnSpace : Pieces -> Vec2 -> List Piece
getPiecesOnSpace pieces spacePosition =
    pieces
        |> Dict.values
        |> List.filter (.position >> (==) spacePosition)


movePieces sourcePos targetPos pieces =
    Dict.map
        (\index piece ->
            --TODO should this be within epsilon?
            if piece.position == sourcePos then
                { piece | position = targetPos }
            else
                piece
        )
        pieces


removePiecesAtPosition position pieces =
    Dict.filter
        (\index piece ->
            --TODO should this be within epsilon?
            piece.position /= position
        )
        pieces
