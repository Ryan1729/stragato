module Model exposing (..)

import Mouse
import Math.Vector2 as V2 exposing (Vec2, vec2)


type alias Model =
    { pieceDrag :
        Maybe PieceDrag
        -- width = 8,
        -- height = 6,
    , piecePosition : Vec2
    }


type alias PieceDrag =
    { --pieceID : Int
      start : Mouse.Position
    }


defaultState =
    { pieceDrag = Nothing
    , piecePosition = vec2 400 400
    }


init =
    defaultState ! []
