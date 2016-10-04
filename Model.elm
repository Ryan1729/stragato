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


type alias Model =
    { pieceSelected : Maybe Int
    , pieces : Pieces
    , spaces : Spaces
    , seed : Seed
    , tabIndex : Int
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
    , exportModel = defaultExportModel
    , gameResult = TBD
    , ignoreGameResult = False
    , debug = True
    , showSpaceOutlines = True
    , allowMovingAllPieces = False
    , windowSize = { width = 600, height = 600 }
    , mdl = Material.model
    }


type alias ExportModel =
    { gridWidth : Int
    , gridHeight : Int
    , spaceDeck : List SpaceType
    , pieceDeck : List ProtoPiece
    , moveTypeDeck : List MoveType
    , pieceAppearances : PieceAppearances
    , gameEndCons : GameEndCons
    , viewScale : Float
    }


defaultExportModel =
    { gridWidth = defaultWidth
    , gridHeight = defaultHeight
    , spaceDeck = defaultSpaceDeck
    , pieceDeck = defaultPieceTypeDeck
    , moveTypeDeck = defaultMoveTypeDeck
    , pieceAppearances = defaultpieceAppearances
    , gameEndCons =
        GameEndCons
            (NoPiecesOfGivenTypeCanMove
                (PieceType Pieces.NoEffect
                    Pieces.Both
                    Pieces.Unoccupied
                )
            )
            (NoPiecesControlledBy Player)
    , viewScale = 1.0
    }


defaultWidth =
    5


defaultHeight =
    5



--TODO: should draws be possible?


type GameResult
    = Win
    | Loss
    | TBD



-- GameEndCons WinPredicate LossPredicate


type GameEndCons
    = GameEndCons GamePredicate GamePredicate


type GamePredicate
    = NoPiecesControlledBy Controller
    | NoPiecesStrictlyControlledBy Controller
    | NoPiecesOfGivenTypeCanMove PieceType


gamePredicatePossibilities =
    [ NoPiecesControlledBy Player
    , NoPiecesControlledBy Computer
    , NoPiecesControlledBy Both
    , NoPiecesControlledBy None
    , NoPiecesStrictlyControlledBy Player
    , NoPiecesStrictlyControlledBy Computer
    , NoPiecesStrictlyControlledBy Both
    , NoPiecesStrictlyControlledBy None
    ]
        ++ List.map NoPiecesOfGivenTypeCanMove Pieces.actualPieceTypePossibilities


decrementGamePredicate : GamePredicate -> GamePredicate
decrementGamePredicate predicate =
    (Extras.indexOf gamePredicatePossibilities predicate
        |> Maybe.map (\index -> (index - 1) % List.length gamePredicatePossibilities)
    )
        `Maybe.andThen` (\index -> List.head <| List.drop index gamePredicatePossibilities)
        |> Maybe.withDefault predicate


incrementGamePredicate : GamePredicate -> GamePredicate
incrementGamePredicate predicate =
    (Extras.indexOf gamePredicatePossibilities predicate
        |> Maybe.map (\index -> (index + 1) % List.length gamePredicatePossibilities)
    )
        `Maybe.andThen` (\index -> List.head <| List.drop index gamePredicatePossibilities)
        |> Maybe.withDefault predicate


defaultSpaceDeck =
    [ Green
    , Green
    , Red
    , Red
    , EmptySpace
    ]


defaultSpaces =
    fst
        <| makeSpaces defaultWidth
            defaultHeight
            defaultSpaceDeck
            (Random.initialSeed -42)


defaultPieceTypeDeck =
    Pieces.actualPieceTypePossibilities
        |> List.filter (\p -> p.moveType == Pieces.AnySpace)
        |> List.map Pieces.ActualPiece
        |> (++)
            [ NoPiece
            , NoPiece
            ]



-- Pieces.protoPiecePossibilities
--     ++ [ NoPiece
--         , NoPiece
--        ]


defaultMoveTypeDeck =
    Pieces.moveTypePossibilities


defaultPieces : Pieces
defaultPieces =
    makePieces defaultSpaces defaultPieceTypeDeck defaultMoveTypeDeck (Random.initialSeed -421)
        |> fst


defaultpieceAppearances : PieceAppearances
defaultpieceAppearances =
    Pieces.actualPieceTypePossibilities
        |> List.map PieceAppearances.pairWithAppearance
        |> PieceAppearances.fromList


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


getWinConString : Model -> String
getWinConString model =
    case model.exportModel.gameEndCons of
        GameEndCons winCon _ ->
            toString winCon


getLossConString : Model -> String
getLossConString model =
    case model.exportModel.gameEndCons of
        GameEndCons _ lossCon ->
            toString lossCon


decrementWinCon : GameEndCons -> GameEndCons
decrementWinCon gameEndCons =
    case gameEndCons of
        GameEndCons winCon lossCon ->
            GameEndCons (decrementGamePredicate winCon) lossCon


incrementWinCon : GameEndCons -> GameEndCons
incrementWinCon gameEndCons =
    case gameEndCons of
        GameEndCons winCon lossCon ->
            GameEndCons (incrementGamePredicate winCon) lossCon


decrementLossCon : GameEndCons -> GameEndCons
decrementLossCon gameEndCons =
    case gameEndCons of
        GameEndCons winCon lossCon ->
            GameEndCons winCon (decrementGamePredicate lossCon)


incrementLossCon : GameEndCons -> GameEndCons
incrementLossCon gameEndCons =
    case gameEndCons of
        GameEndCons winCon lossCon ->
            GameEndCons winCon (incrementGamePredicate lossCon)
