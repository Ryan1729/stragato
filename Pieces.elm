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


type MoveType
    = Occupied
    | Unoccupied
    | AnySpace


moveTypePossibilities =
    [ Occupied
    , Unoccupied
    , AnySpace
    ]


type Controller
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


type Shape
    = Star
    | WeirdThing
    | Triangle
    | Eye
    | Petals
    | TwistedPlus
    | Fangs


shapePossibilities =
    [ Star, WeirdThing, Triangle, Eye, Petals, Fangs, TwistedPlus ]


type ProtoPiece
    = ActualPiece PieceType
    | NoPiece


type alias PieceType =
    { shape : Shape
    , controller : Controller
    , moveType : MoveType
    }


actualPieceTypePossibilities =
    List.concatMap
        (\shape ->
            List.concatMap
                (\controller ->
                    List.map (PieceType shape controller)
                        moveTypePossibilities
                )
                pieceControllabilityPossibilities
        )
        shapePossibilities


protoPiecePossibilities =
    List.map ActualPiece actualPieceTypePossibilities ++ [ NoPiece ]


controllabiltyCount : Controller -> Pieces -> Int
controllabiltyCount controlability =
    case controlability of
        Player ->
            playerControlledCount

        Computer ->
            cpuControlledCount

        Both ->
            bothControlledCount

        None ->
            noneControlledCount


strictControllabiltyCount : Controller -> Pieces -> Int
strictControllabiltyCount controlability =
    case controlability of
        Player ->
            strictPlayerControlledCount

        Computer ->
            strictCpuControlledCount

        Both ->
            bothControlledCount

        None ->
            noneControlledCount


isComputerControllable : Piece -> Bool
isComputerControllable piece =
    let
        controllability =
            piece.pieceType.controller
    in
        controllability == Computer || controllability == Both


isPlayerControllable : Piece -> Bool
isPlayerControllable piece =
    let
        controllability =
            piece.pieceType.controller
    in
        controllability == Player || controllability == Both


isStrictlyComputerControllable : Piece -> Bool
isStrictlyComputerControllable piece =
    let
        controllability =
            piece.pieceType.controller
    in
        controllability == Computer


isStrictlyPlayerControllable : Piece -> Bool
isStrictlyPlayerControllable piece =
    let
        controllability =
            piece.pieceType.controller
    in
        controllability == Player


isBothControllable : Piece -> Bool
isBothControllable piece =
    let
        controllability =
            piece.pieceType.controller
    in
        controllability == Both


isNoneControllable : Piece -> Bool
isNoneControllable piece =
    let
        controllability =
            piece.pieceType.controller
    in
        controllability == None


cpuControlledCount : Pieces -> Int
cpuControlledCount =
    countPiecesThatFufillPredicate isComputerControllable


playerControlledCount : Pieces -> Int
playerControlledCount =
    countPiecesThatFufillPredicate isPlayerControllable


bothControlledCount : Pieces -> Int
bothControlledCount =
    countPiecesThatFufillPredicate isBothControllable


noneControlledCount : Pieces -> Int
noneControlledCount =
    countPiecesThatFufillPredicate isNoneControllable


strictCpuControlledCount =
    countPiecesThatFufillPredicate isStrictlyComputerControllable


strictPlayerControlledCount =
    countPiecesThatFufillPredicate isStrictlyPlayerControllable


countPiecesThatFufillPredicate : (Piece -> Bool) -> Pieces -> Int
countPiecesThatFufillPredicate predicate pieces =
    Dict.foldl
        (\index value acc ->
            if predicate value then
                acc + 1
            else
                acc
        )
        0
        pieces


setPieceLocation : Int -> Vec2 -> Pieces -> Pieces
setPieceLocation pieceID position pieces =
    Dict.update pieceID
        (Maybe.map
            (\piece ->
                { piece | position = position }
            )
        )
        pieces


addPiece : Piece -> Pieces -> Pieces
addPiece piece pieces =
    let
        lowestFreeIndex =
            Extras.getLowestAbsentInt <| Dict.keys pieces
    in
        Dict.insert lowestFreeIndex piece pieces


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
        piecesOnSpace == []


getCPUMovablePieces : Pieces -> List Int
getCPUMovablePieces pieces =
    Dict.filter (Extras.ignoreFirstArg isComputerControllable) pieces
        |> Dict.keys


getPlayerMovablePieces : Pieces -> List Int
getPlayerMovablePieces pieces =
    Dict.filter (Extras.ignoreFirstArg isPlayerControllable) pieces
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
