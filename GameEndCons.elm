module GameEndCons exposing (..)

import Spaces exposing (Spaces, Space, SpaceType(..))
import Pieces exposing (Pieces, Piece, PieceType, Controller(..), MoveType(..), ProtoPiece(..))
import Extras


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


getWinConString : GameEndCons -> String
getWinConString gameEndCons =
    case gameEndCons of
        GameEndCons winCon _ ->
            toString winCon


getLossConString : GameEndCons -> String
getLossConString gameEndCons =
    case gameEndCons of
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
