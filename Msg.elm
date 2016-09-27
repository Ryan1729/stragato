module Msg exposing (..)

import Mouse
import Material
import Spaces exposing (SpaceType)
import Pieces exposing (ProtoPiece)


type Msg
    = HitTable
    | SelectPiece Int
    | ClearPieceSelection
    | Animate Float
    | MovePiece Int ( Int, Int )
    | GetSeed Float
    | GenerateBoard
    | SelectTab Int
    | Mdl (Material.Msg Msg)
    | SpaceDeckIncrement SpaceType
    | SpaceDeckDecrement SpaceType
    | PieceDeckIncrement ProtoPiece
    | PieceDeckDecrement ProtoPiece
    | IncrementGridWidth
    | DecrementGridWidth
    | IncrementGridHeight
    | DecrementGridHeight
    | IncrementViewScale
    | DecrementViewScale
    | ToggleSpaceOutlines
    | ToggleSelfMoves
    | ToggleAllowMovingAllPieces
    | ToggleIgnoreGameResult
    | DecrementWinCon
    | IncrementWinCon
    | DecrementLossCon
    | IncrementLossCon
    | Resize { width : Int, height : Int }
    | MakeAIMove
