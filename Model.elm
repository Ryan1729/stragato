module Model exposing (..)

import Mouse
import Math.Vector2 as V2 exposing (Vec2, vec2, getX, getY, add, scale)
import Array exposing (Array)
import Array.Extra
import Random exposing (Seed)
import Material
import PlayfieldComponents exposing (Piece, PieceType(..), Spaces, SpaceType(..))


type alias Model =
    { pieceSelected : Maybe Int
    , pieces : Array Piece
    , spaces : Spaces
    , gridWidth : Int
    , gridHeight : Int
    , spaceDeck : List SpaceType
    , pieceDeck : List PieceType
    , seed : Seed
    , tabIndex : Int
    , debug : Bool
    , showSpaceOutlines : Bool
    , viewScale : Float
    , mdl : Material.Model
    }


defaultWidth =
    4


defaultHeight =
    5


defaultSpaceDeck =
    [ Green
    , Green
    , Red
    , Red
    , EmptySpace
    , EmptySpace
    , EmptySpace
    , EmptySpace
    ]


defaultSpaces =
    fst
        <| PlayfieldComponents.makeSpaces defaultWidth
            defaultHeight
            defaultSpaceDeck
            (Random.initialSeed -42)


defaultPieceDeck =
    [ Star
    , Star
    , Star
    , WeirdThing
    , WeirdThing
    , NoPiece
    , NoPiece
    , NoPiece
    , NoPiece
    , NoPiece
    , NoPiece
    , NoPiece
    ]


defaultPieces =
    fst <| PlayfieldComponents.makePieces defaultSpaces defaultPieceDeck (Random.initialSeed -421)


defaultState =
    { pieceSelected = Nothing
    , pieces = defaultPieces
    , spaces = defaultSpaces
    , seed = (Random.initialSeed 42)
    , gridWidth = defaultWidth
    , gridHeight = defaultHeight
    , spaceDeck = defaultSpaceDeck
    , pieceDeck = defaultPieceDeck
    , tabIndex = 0
    , debug = True
    , showSpaceOutlines = False
    , viewScale = 1.0
    , mdl = Material.model
    }
