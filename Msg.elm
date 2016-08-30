module Msg exposing (..)

import Mouse


type Msg
    = PlayClack
    | SelectPiece Int
    | Animate Float
    | MovePiece Int Int
    | GetSeed Float
    | GenerateBoard
