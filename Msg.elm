module Msg exposing (..)

import Mouse
import Material
import Spaces exposing (SpaceType)
import Pieces exposing (ProtoPiece, PieceType)
import Math.Vector2 exposing (Vec2)
import PieceAppearances exposing (Icon)


type Msg
    = HitTable
    | SelectPiece Int
    | ClearPieceSelection
    | Animate Float
    | MovePiece Int ( Int, Int )
    | GetSeed Float
    | GenerateBoard
    | Resize { width : Int, height : Int }
    | MakeAIMove
    | SelectTab Int
    | Mdl (Material.Msg Msg)
    | ToggleSpaceOutlines
    | ToggleAllowMovingAllPieces
    | ToggleIgnoreGameResult
    | UpdateExportModel ExportMsg
    | SaveAs
    | NoOp


type ExportMsg
    = SpaceDeckIncrement SpaceType Int
    | SpaceDeckDecrement SpaceType Int
    | PieceDeckIncrement ProtoPiece Int
    | PieceDeckDecrement ProtoPiece Int
    | IncrementGridWidth
    | DecrementGridWidth
    | IncrementGridHeight
    | DecrementGridHeight
    | IncrementViewScale
    | DecrementViewScale
    | DecrementWinCon
    | IncrementWinCon
    | DecrementLossCon
    | IncrementLossCon
    | UpdateColour PieceType String
    | EditPoints PieceType (List Vec2)
    | SetIcon Icon PieceType
