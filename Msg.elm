module Msg exposing (..)

import Mouse
import Material
import Spaces exposing (SpaceType)
import Pieces exposing (ProtoPiece, PieceType)
import Math.Vector2 exposing (Vec2)
import PieceAppearances


type Msg
    = Animate Float
    | GetSeed Float
    | SelectTab Int
    | SelectPieceDeckTab Int
    | Mdl (Material.Msg Msg)
    | ToggleSpaceOutlines
    | ToggleAllowMovingAllPieces
    | ToggleIgnoreGameResult
    | UpdateExportModel ExportMsg
    | SaveAs
    | Load
    | Export
    | RecieveLoadedFile String
    | NoOp


type ExportMsg
    = SpaceDeckIncrement SpaceType Int
    | SpaceDeckDecrement SpaceType Int
    | PieceDeckIncrement ProtoPiece Int
    | PieceDeckDecrement ProtoPiece Int
    | UpdateGridWidth String
    | UpdateGridHeight String
    | UpdateViewScale String
    | DecrementWinCon
    | IncrementWinCon
    | DecrementSubWinCon
    | IncrementSubWinCon
    | DecrementLossCon
    | IncrementLossCon
    | DecrementSubLossCon
    | IncrementSubLossCon
    | UpdateColour PieceType String
    | EditPoints PieceType (List Vec2)
