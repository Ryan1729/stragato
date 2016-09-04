module Msg exposing (..)

import Mouse
import Material
import PlayfieldComponents exposing (SpaceType)


type Msg
    = PlayClack
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
