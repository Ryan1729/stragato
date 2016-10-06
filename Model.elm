module Model exposing (..)

import Mouse
import Math.Vector2 as V2 exposing (Vec2, vec2, getX, getY, add, scale)
import Random exposing (Seed)
import Material
import Points
import Extras
import Dict exposing (Dict)
import Spaces exposing (Spaces, Space, SpaceType(..))
import Pieces exposing (Pieces, Piece, PieceType, Controller(..), MoveType(..), ProtoPiece(..))
import Deck
import PieceAppearances exposing (PieceAppearances, Appearance)
import GameEndCons exposing (GameEndCons(..), GamePredicate(..))
import ExportModel exposing (ExportModel)


type alias Model =
    { pieceSelected : Maybe Int
    , pieces : Pieces
    , spaces : Spaces
    , seed : Seed
    , tabIndex : Int
    , pieceDeckTabIndex : Int
    , exportModel : ExportModel
    , gameResult : GameResult
    , ignoreGameResult : Bool
    , debug : Bool
    , showSpaceOutlines : Bool
    , allowMovingAllPieces : Bool
    , windowSize : { width : Int, height : Int }
    , mdl : Material.Model
    }


defaultState =
    { pieceSelected = Nothing
    , pieces = defaultPieces
    , spaces = defaultSpaces
    , seed = (Random.initialSeed 42)
    , tabIndex = 0
    , pieceDeckTabIndex = 0
    , exportModel = ExportModel.defaultExportModel
    , gameResult = TBD
    , ignoreGameResult = False
    , debug = True
    , showSpaceOutlines = True
    , allowMovingAllPieces = False
    , windowSize = { width = 600, height = 600 }
    , mdl = Material.model
    }



--TODO: should draws be possible?


type GameResult
    = Win
    | Loss
    | TBD


defaultSpaces =
    fst
        <| makeSpaces ExportModel.defaultWidth
            ExportModel.defaultHeight
            ExportModel.defaultSpaceDeck
            (Random.initialSeed -42)


defaultPieces : Pieces
defaultPieces =
    makePieces defaultSpaces
        ExportModel.defaultPieceTypeDeck
        ExportModel.defaultMoveTypeDeck
        (Random.initialSeed -421)
        |> fst


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


makePieces : Spaces -> List ProtoPiece -> List MoveType -> Seed -> ( Pieces, Seed )
makePieces spaces protoPieceDeck moveTypeDeck seed =
    let
        filteredPositions =
            Spaces.getActualSpacePositions spaces

        pieceAmount =
            List.length filteredPositions

        ( pieceTypes, postPieceTypesSeed ) =
            Deck.fillListFromDeck NoPiece
                protoPieceDeck
                pieceAmount
                seed

        ( moveTypes, newSeed ) =
            Deck.fillListFromDeck AnySpace
                moveTypeDeck
                pieceAmount
                postPieceTypesSeed

        pieces =
            List.map2 attemptPiece pieceTypes filteredPositions
                |> List.filterMap identity
                |> List.indexedMap (,)
                |> Dict.fromList
    in
        ( pieces
        , newSeed
        )


attemptPiece : ProtoPiece -> Vec2 -> Maybe Piece
attemptPiece protoPiece position =
    case protoPiece of
        ActualPiece pieceType ->
            Piece pieceType position
                |> Just

        NoPiece ->
            Nothing


canMove : Model -> Bool
canMove model =
    model.ignoreGameResult || model.gameResult == TBD
