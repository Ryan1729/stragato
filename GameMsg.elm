module GameMsg exposing (..)


type Msg
    = HitTable
    | SelectPiece Int
    | ClearPieceSelection
    | MovePiece Int ( Int, Int )
    | GenerateBoard
    | MakeAIMove
    | GetSeed Float
