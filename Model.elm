module Model exposing (..)

import Mouse
import Math.Vector2 as V2 exposing (Vec2, vec2)


type alias Model =
    { pieceSelected : Maybe Int
    , piecePosition : Vec2
    , width : Int
    , height : Int
    , pieceList : List Piece
    }


type alias Piece =
    { pieceType : PieceType
    , position : Vec2
    }


type PieceType
    = Star
    | WeirdThing


defaultState =
    { pieceSelected = Nothing
    , piecePosition = vec2 100 100
    , width = 4
    , height = 5
    , pieceList = [ Piece Star (vec2 280 100), Piece WeirdThing (vec2 100 100) ]
    }


init =
    defaultState ! []
