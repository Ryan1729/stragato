module Model exposing (..)

import Mouse
import Math.Vector2 as V2 exposing (Vec2, vec2, getX, getY, add, scale)
import Random exposing (Seed)
import Material
import Points
import Extras
import Dict exposing (Dict)
import Spaces exposing (Spaces, Space, SpaceType(..))
import Pieces exposing (Piece, PieceType(..))
import Deck


type alias Model =
    { pieceSelected : Maybe Int
    , pieces : Dict Int Piece
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
        <| makeSpaces defaultWidth
            defaultHeight
            defaultSpaceDeck
            (Random.initialSeed -42)


defaultPieceDeck =
    Pieces.pieceTypePossibilities
        ++ [ NoPiece
           , NoPiece
           , NoPiece
           , NoPiece
           ]


defaultPieces : Dict Int Piece
defaultPieces =
    makePieces defaultSpaces defaultPieceDeck (Random.initialSeed -421)
        |> fst


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
    , showSpaceOutlines = True
    , viewScale = 1.0
    , mdl = Material.model
    }


makeSpaces : Int -> Int -> List SpaceType -> Seed -> ( Spaces, Seed )
makeSpaces width height deck seed =
    let
        gridPoints =
            makeGridPoints width height

        ( spaceTypes, newSeed ) =
            Deck.fillListFromDeck EmptySpace deck (List.length gridPoints) seed

        spaces =
            List.map2 putSpaceTogether gridPoints spaceTypes
                |> Dict.fromList
    in
        ( spaces, newSeed )


putSpaceTogether : ( ( Int, Int ), Vec2 ) -> SpaceType -> ( ( Int, Int ), Space )
putSpaceTogether ( index, position ) spaceType =
    ( index, Space position spaceType )


makeGridPoints : Int -> Int -> List ( ( Int, Int ), Vec2 )
makeGridPoints width height =
    Points.hexGrid width height
        |> List.map
            (\( pair, vector ) ->
                ( pair
                , vector
                    |> V2.scale 60
                    |> add (vec2 100 100)
                )
            )


makePieces : Spaces -> List PieceType -> Seed -> ( Dict Int Piece, Seed )
makePieces spaces deck seed =
    let
        filteredPositions =
            Spaces.getActualSpacePositions spaces

        ( pieceTypes, newSeed ) =
            Deck.fillListFromDeck NoPiece
                deck
                (List.length filteredPositions)
                seed

        pieces =
            List.map2 Piece pieceTypes filteredPositions
                |> List.indexedMap (,)
                |> Dict.fromList
    in
        ( pieces
        , newSeed
        )
