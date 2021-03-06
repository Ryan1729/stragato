module Model exposing (..)

import Mouse
import Math.Vector2 as V2 exposing (Vec2, vec2, getX, getY, add, scale)
import Random.Pcg as Random exposing (Seed)
import Material
import Points
import Extras
import Dict exposing (Dict)
import Spaces exposing (Spaces, Space, SpaceType(..))
import Pieces exposing (Pieces, Piece, PieceType, Controller(..), MovePattern, ProtoPiece(..))
import Deck
import PieceAppearances exposing (PieceAppearances, Appearance)
import GameEndCons exposing (GameEndCons(..), GamePredicate(..))
import ExportModel exposing (ExportModel)
import TransferModel exposing (TransferModel)
import Hexagons


type alias Model =
    { pieces : Pieces
    , spaces : Spaces
    , seed : Seed
    , tabIndex : Int
    , pieceDeckTabIndex : Int
    , exportModel : ExportModel
    , ignoreGameResult : Bool
    , showSpaceOutlines : Bool
    , allowMovingAllPieces : Bool
    , showFileInput : Bool
    , showPieceEditor : Bool
    , editingPieceType : PieceType
    , mdl : Material.Model
    }


defaultState =
    { pieces = defaultPieces
    , spaces = defaultSpaces
    , seed = (Random.initialSeed 42)
    , tabIndex = 0
    , pieceDeckTabIndex = 0
    , exportModel = ExportModel.defaultExportModel
    , ignoreGameResult = False
    , showSpaceOutlines = True
    , allowMovingAllPieces = False
    , showFileInput = False
    , showPieceEditor = False
    , editingPieceType = Pieces.defaultPieceType
    , mdl = Material.model
    }


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
    Hexagons.grid width height
        |> List.map
            (\( pair, vector ) ->
                ( pair
                , vector
                    |> V2.scale 60
                    |> add (vec2 100 100)
                )
            )


makePieces : Spaces -> List ProtoPiece -> Seed -> ( Pieces, Seed )
makePieces spaces protoPieceDeck seed =
    let
        filteredPositions =
            Spaces.getActualSpacePositions spaces

        pieceAmount =
            List.length filteredPositions

        ( pieceTypes, newSeed ) =
            Deck.fillListFromDeck NoPiece
                protoPieceDeck
                pieceAmount
                seed

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


modelToTransferModel : Model -> TransferModel
modelToTransferModel model =
    { exportModel = model.exportModel
    , pieces = Just model.pieces
    , spaces = Just model.spaces
    , ignoreGameResult = model.ignoreGameResult
    , showSpaceOutlines = model.showSpaceOutlines
    , allowMovingAllPieces = model.allowMovingAllPieces
    }


modelToTransferModelWithoutPieces : Model -> TransferModel
modelToTransferModelWithoutPieces model =
    { exportModel = model.exportModel
    , pieces = Nothing
    , spaces = Nothing
    , ignoreGameResult = model.ignoreGameResult
    , showSpaceOutlines = model.showSpaceOutlines
    , allowMovingAllPieces = model.allowMovingAllPieces
    }
