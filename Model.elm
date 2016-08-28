module Model exposing (..)

import Mouse
import Math.Vector2 as V2 exposing (Vec2, vec2)


type alias Model =
    { pieceDrag : Maybe PieceDrag
    , piecePosition : Vec2
    , width : Int
    , height : Int
    }


type alias PieceDrag =
    { --pieceID : Int
      position : Mouse.Position
    }


defaultState =
    { pieceDrag = Nothing
    , piecePosition = vec2 400 400
    , width = 4
    , height = 5
    }


init =
    defaultState ! []
