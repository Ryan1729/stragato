module Msg exposing (..)

import Mouse


type Msg
    = PlayClack
    | PieceDragStart Mouse.Position
    | PieceDragMove Mouse.Position
    | PieceDragEnd Mouse.Position
    | Animate Float
