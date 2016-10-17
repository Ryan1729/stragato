module GameEndCons exposing (..)

import Spaces exposing (Spaces, Space, SpaceType(..))
import Pieces exposing (Pieces, Piece, PieceType, Controller(..), MoveOccupancy(..), ProtoPiece(..))
import Extras
import String


-- GameEndCons WinPredicate LossPredicate


type GameEndCons
    = GameEndCons GamePredicate GamePredicate


type GamePredicate
    = NoPiecesControlledBy Controller
    | NoPiecesStrictlyControlledBy Controller
    | NoPiecesOfGivenTypeCanMove PieceType


gamePredicatePossibilitesPossibilites =
    [ noPiecesControlledByPossibilities
    , noPiecesStrictlyControlledByPossibilities
    , noPiecesOfGivenTypeCanMovePossibilities
    ]


noPiecesControlledByPossibilities =
    [ NoPiecesControlledBy Player
    , NoPiecesControlledBy Computer
    , NoPiecesControlledBy Both
    , NoPiecesControlledBy None
    ]


noPiecesStrictlyControlledByPossibilities =
    [ NoPiecesStrictlyControlledBy Player
    , NoPiecesStrictlyControlledBy Computer
    , NoPiecesStrictlyControlledBy Both
    , NoPiecesStrictlyControlledBy None
    ]


noPiecesOfGivenTypeCanMovePossibilities =
    List.map NoPiecesOfGivenTypeCanMove Pieces.actualPieceTypePossibilities


decrementSubGamePredicate : GamePredicate -> GamePredicate
decrementSubGamePredicate predicate =
    case predicate of
        NoPiecesControlledBy _ ->
            decrementInList noPiecesControlledByPossibilities predicate

        NoPiecesStrictlyControlledBy _ ->
            decrementInList noPiecesStrictlyControlledByPossibilities predicate

        NoPiecesOfGivenTypeCanMove _ ->
            decrementInList noPiecesOfGivenTypeCanMovePossibilities predicate


decrementInList : List a -> a -> a
decrementInList list thing =
    (Extras.indexOf list thing
        |> Maybe.map (\index -> (index - 1) % List.length list)
    )
        `Maybe.andThen` (\index -> List.head <| List.drop index list)
        |> Maybe.withDefault thing


incrementSubGamePredicate : GamePredicate -> GamePredicate
incrementSubGamePredicate predicate =
    case predicate of
        NoPiecesControlledBy _ ->
            incrementInList noPiecesControlledByPossibilities predicate

        NoPiecesStrictlyControlledBy _ ->
            incrementInList noPiecesStrictlyControlledByPossibilities predicate

        NoPiecesOfGivenTypeCanMove _ ->
            incrementInList noPiecesOfGivenTypeCanMovePossibilities predicate


incrementInList : List a -> a -> a
incrementInList list thing =
    (Extras.indexOf list thing
        |> Maybe.map (\index -> (index + 1) % List.length list)
    )
        `Maybe.andThen` (\index -> List.head <| List.drop index list)
        |> Maybe.withDefault thing


incrementGamePredicate : GamePredicate -> GamePredicate
incrementGamePredicate =
    crementGamePredicateMaker incrementInList


decrementGamePredicate : GamePredicate -> GamePredicate
decrementGamePredicate =
    crementGamePredicateMaker decrementInList


crementGamePredicateMaker :
    (List (List GamePredicate)
     -> List GamePredicate
     -> List GamePredicate
    )
    -> GamePredicate
    -> GamePredicate
crementGamePredicateMaker crementer predicate =
    case predicate of
        NoPiecesControlledBy _ ->
            crementer gamePredicatePossibilitesPossibilites noPiecesControlledByPossibilities
                |> List.head
                |> Maybe.withDefault predicate

        NoPiecesStrictlyControlledBy _ ->
            crementer gamePredicatePossibilitesPossibilites noPiecesStrictlyControlledByPossibilities
                |> List.head
                |> Maybe.withDefault predicate

        NoPiecesOfGivenTypeCanMove _ ->
            crementer gamePredicatePossibilitesPossibilites noPiecesOfGivenTypeCanMovePossibilities
                |> List.head
                |> Maybe.withDefault predicate


getWinConString : GameEndCons -> String
getWinConString gameEndCons =
    case gameEndCons of
        GameEndCons winCon _ ->
            gamePredicateToString winCon


gamePredicateToString con =
    case con of
        NoPiecesControlledBy _ ->
            toString con

        NoPiecesStrictlyControlledBy _ ->
            toString con

        NoPiecesOfGivenTypeCanMove pieceType ->
            "NoPiecesOfGivenTypeCanMove"
                :: Pieces.pieceTypeToStringList pieceType
                |> String.join " "


getLossConString : GameEndCons -> String
getLossConString gameEndCons =
    case gameEndCons of
        GameEndCons _ lossCon ->
            gamePredicateToString lossCon


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


decrementSubWinCon : GameEndCons -> GameEndCons
decrementSubWinCon gameEndCons =
    case gameEndCons of
        GameEndCons winCon lossCon ->
            GameEndCons (decrementSubGamePredicate winCon) lossCon


incrementSubWinCon : GameEndCons -> GameEndCons
incrementSubWinCon gameEndCons =
    case gameEndCons of
        GameEndCons winCon lossCon ->
            GameEndCons (incrementSubGamePredicate winCon) lossCon


decrementSubLossCon : GameEndCons -> GameEndCons
decrementSubLossCon gameEndCons =
    case gameEndCons of
        GameEndCons winCon lossCon ->
            GameEndCons winCon (decrementSubGamePredicate lossCon)


incrementSubLossCon : GameEndCons -> GameEndCons
incrementSubLossCon gameEndCons =
    case gameEndCons of
        GameEndCons winCon lossCon ->
            GameEndCons winCon (incrementSubGamePredicate lossCon)
