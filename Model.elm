module Model exposing (..)

import Mouse
import Math.Vector2 as V2 exposing (Vec2, vec2, getX, getY, add, scale)
import Points
import Array exposing (Array)


type alias Model =
    { pieceSelected : Maybe Int
    , piecePosition : Vec2
    , width : Int
    , height : Int
    , pieceList : List Piece
    , spaces : Spaces
    }


type alias Piece =
    { pieceType : PieceType
    , position : Vec2
    }


type PieceType
    = Star
    | WeirdThing


type alias Spaces =
    { positions : Array Vec2
    }


gridPoints =
    Points.hexGrid 4 5
        |> List.map (add (vec2 100 100) << V2.scale 60)
        |> Array.fromList


defaultState =
    { pieceSelected = Nothing
    , piecePosition = vec2 100 100
    , width = 4
    , height = 5
    , pieceList = [ Piece Star (vec2 280 100), Piece WeirdThing (vec2 100 100) ]
    , spaces = Spaces gridPoints
    }


init =
    defaultState ! []
