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
    | Mdl (Material.Msg Msg)
