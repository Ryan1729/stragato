module Model exposing (..)

import Mouse
import Math.Vector2 as V2 exposing (Vec2, vec2, getX, getY, add, scale)
import Points
import Array exposing (Array)
import Random exposing (Seed)


type alias Model =
    { pieceSelected : Maybe Int
    , piecePosition : Vec2
    , pieceList : List Piece
    , spaces : Spaces
    , seed : Seed
    , debug : Bool
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
    , types : Array SpaceType
    }


type SpaceType
    = Green
    | Red
    | Empty


makeGridPoints width height =
    Points.hexGrid width height
        |> List.map (add (vec2 100 100) << V2.scale 60)
        |> Array.fromList


defaultState =
    { pieceSelected = Nothing
    , piecePosition = vec2 100 100
    , pieceList = [ Piece Star (vec2 280 100), Piece WeirdThing (vec2 100 100) ]
    , spaces = makeSpaces 4 5 [ Green, Green, Red, Red, Empty ] (Random.initialSeed -42)
    , seed = (Random.initialSeed 42)
    , debug = True
    }



--TODO add generate board button


makeSpaces : Int -> Int -> List SpaceType -> Seed -> Spaces
makeSpaces width height deck seed =
    let
        gridpoints =
            makeGridPoints width height

        --TODO replace this ugly hack with actual drawing from deck and reshuffling
        spaceTypes =
            List.repeat (Array.length gridpoints) deck
                |> List.concat
                |> List.take (Array.length gridpoints)
                |> Array.fromList
    in
        Spaces gridpoints spaceTypes
