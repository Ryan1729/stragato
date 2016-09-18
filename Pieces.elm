module Pieces exposing (..)

import Math.Vector2 exposing (Vec2)
import Dict exposing (Dict)
import Extras


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


getControllability : PieceType -> PieceControllability
getControllability pieceType =
    case pieceType of
        Star controllability ->
            controllability

        WeirdThing controllability ->
            controllability

        Triangle controllability ->
            controllability

        Eye controllability ->
            controllability

        NoPiece ->
            None


isComputerControllable : Piece -> Bool
isComputerControllable piece =
    let
        controllability =
            getControllability piece.pieceType
    in
        controllability == Computer || controllability == Both


isPlayerControllable : Piece -> Bool
isPlayerControllable piece =
    let
        controllability =
            getControllability piece.pieceType
    in
        controllability == Player || controllability == Both


isActualPiece : Piece -> Bool
isActualPiece piece =
    piece.pieceType /= NoPiece


setPieceLocation : Int -> Vec2 -> Pieces -> Pieces
setPieceLocation pieceID position pieces =
    Dict.update pieceID
        (Maybe.map
            (\piece ->
                { piece | position = position }
            )
        )
        pieces


getPiecesAtPosition : Pieces -> Vec2 -> List Piece
getPiecesAtPosition pieces position =
    pieces
        |> Dict.values
        |> List.filter (.position >> (==) position)


noPiecesAtPosition : Pieces -> Vec2 -> Bool
noPiecesAtPosition pieces position =
    let
        piecesOnSpace =
            getPiecesAtPosition pieces position
    in
        piecesOnSpace
            |> List.filter isActualPiece
            |> (==) []


getCPUMovablePieces : Pieces -> List Int
getCPUMovablePieces pieces =
    Dict.filter (Extras.ignoreFirstArg isComputerControllable) pieces
        |> Dict.keys


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


filterOutNonActualPieces : Pieces -> Pieces
filterOutNonActualPieces pieces =
    Dict.filter (Extras.ignoreFirstArg isActualPiece) pieces
