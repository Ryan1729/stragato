module Msg exposing (..)

import Mouse
import Material
import PlayfieldComponents exposing (SpaceType, PieceType)


type Msg
    = HitTable
    | SelectPiece Int
    | ClearPieceSelection
    | Animate Float
    | MovePiece Int Int
    | GetSeed Float
    | GenerateBoard
    | SelectTab Int
    | Mdl (Material.Msg Msg)
    | SpaceDeckIncrement SpaceType
    | SpaceDeckDecrement SpaceType
    | PieceDeckIncrement PieceType
    | PieceDeckDecrement PieceType
    | IncrementGridWidth
    | DecrementGridWidth
    | IncrementGridHeight
    | DecrementGridHeight
    | IncrementViewScale
    | DecrementViewScale
    | ToggleSpaceOutlines
