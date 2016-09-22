module Model exposing (..)

import Mouse
import Math.Vector2 as V2 exposing (Vec2, vec2, getX, getY, add, scale)
import Random exposing (Seed)
import Material
import Points
import Extras
import Dict exposing (Dict)
import Spaces exposing (Spaces, Space, SpaceType(..))
import Pieces exposing (Pieces, Piece, PieceType(..), PieceControllability(..))
import Deck


type alias Model =
    { allowSelfMoves : Bool
    , pieceSelected : Maybe Int
    , pieces : Pieces
    , spaces : Spaces
    , gridWidth : Int
    , gridHeight : Int
    , spaceDeck : List SpaceType
    , pieceDeck : List PieceType
    , seed : Seed
    , tabIndex : Int
    , gameResult : GameResult
    , ignoreGameResult : Bool
    , gameEndCons : GameEndCons
    , debug : Bool
    , showSpaceOutlines : Bool
    , allowMovingAllPieces : Bool
    , viewScale : Float
    , windowSize : { width : Int, height : Int }
    , mdl : Material.Model
    }


defaultState =
    { allowSelfMoves = False
    , pieceSelected = Nothing
    , pieces = defaultPieces
    , spaces = defaultSpaces
    , seed = (Random.initialSeed 42)
    , gridWidth = defaultWidth
    , gridHeight = defaultHeight
    , spaceDeck = defaultSpaceDeck
    , pieceDeck = defaultPieceDeck
    , tabIndex = 0
    , gameResult = TBD
    , ignoreGameResult = False
    , gameEndCons = GameEndCons NoCPUPieces NoPlayerPieces
    , debug = True
    , showSpaceOutlines = True
    , allowMovingAllPieces = False
    , viewScale = 1.0
    , windowSize = { width = 600, height = 600 }
    , mdl = Material.model
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
    = NoCPUPieces
    | NoPlayerPieces


decrementGamePredicate : GamePredicate -> GamePredicate
decrementGamePredicate predicate =
    case predicate of
        NoPlayerPieces ->
            NoCPUPieces

        NoCPUPieces ->
            NoPlayerPieces


incrementGamePredicate : GamePredicate -> GamePredicate
incrementGamePredicate predicate =
    case predicate of
        NoCPUPieces ->
            NoPlayerPieces

        NoPlayerPieces ->
            NoCPUPieces


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


defaultPieceDeck =
    Pieces.pieceTypePossibilities
        ++ [ NoPiece
             --  , NoPiece
           ]


defaultPieces : Pieces
defaultPieces =
    makePieces defaultSpaces defaultPieceDeck (Random.initialSeed -421)
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


makePieces : Spaces -> List PieceType -> Seed -> ( Pieces, Seed )
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
                |> Pieces.filterOutNonActualPieces
    in
        ( pieces
        , newSeed
        )


canMove : Model -> Bool
canMove model =
    model.ignoreGameResult || model.gameResult == TBD


getWinConString : Model -> String
getWinConString model =
    case model.gameEndCons of
        GameEndCons winCon _ ->
            toString winCon


getLossConString : Model -> String
getLossConString model =
    case model.gameEndCons of
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
