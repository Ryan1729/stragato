module Msg exposing (..)

import Mouse
import Material


type Msg
    = PlayClack
    | SelectPiece Int
    | Animate Float
    | MovePiece Int Int
    | GetSeed Float
    | GenerateBoard
    | SelectTab Int
    | Mdl (Material.Msg Msg)
