module Model exposing (..)

import Mouse
import Math.Vector2 as V2 exposing (Vec2, vec2)


type alias Model =
    { pieceSelected : Maybe Int
    , piecePosition : Vec2
    , width : Int
    , height : Int
    }


defaultState =
    { pieceSelected = Nothing
    , piecePosition = vec2 100 100
    , width = 4
    , height = 5
    }


init =
    defaultState ! []
