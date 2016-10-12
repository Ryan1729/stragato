module GameMsg exposing (..)

import Json.Decode as Decode


type Msg
    = HitTable
    | SelectPiece Int
    | ClearPieceSelection
    | MovePiece Int ( Int, Int )
    | GenerateBoard
    | MakeAIMove
    | GetSeed Float
    | RecieveEditorFile Decode.Value
