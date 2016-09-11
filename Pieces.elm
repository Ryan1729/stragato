module Pieces exposing (..)

import Math.Vector2 exposing (Vec2)


type alias Piece =
    { pieceType : PieceType
    , position : Vec2
    }


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
        [ Star, WeirdThing, Triangle ]


pieceTypePossibilities =
    controllablePossibilities ++ [ NoPiece ]


isActualPiece : Piece -> Bool
isActualPiece piece =
    piece.pieceType /= NoPiece
