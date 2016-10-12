module GameUpdate exposing (..)

import GameMsg exposing (Msg(..))
import GameModel exposing (Model, GameResult, applyTransferModelToGameModel)
import Model
import Spaces exposing (Spaces, Space, SpaceType(..), SpaceIndex)
import Pieces exposing (Pieces)
import GameEndCons exposing (GamePredicate)
import Dict exposing (Dict)
import Deck
import Movement
import GameEndCons exposing (GameEndCons(..), GamePredicate(..))
import Math.Vector2 as V2 exposing (Vec2, vec2)
import Random exposing (Seed)
import CommonPorts
import CommonUpdate
import TransferModel


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HitTable ->
            { model | pieceSelected = Nothing } ! [ CommonPorts.sound "tableHit" ]

        SelectPiece id ->
            { model | pieceSelected = Just id } ! []

        ClearPieceSelection ->
            { model | pieceSelected = Nothing } ! []

        MovePiece pieceID spaceID ->
            let
                newPieces =
                    Movement.getNewPieces model pieceID spaceID
            in
                { model
                    | pieceSelected = Nothing
                    , pieces = newPieces
                    , gameResult = getGameResult model newPieces
                }
                    ! [ CommonPorts.sound "clack" ]

        GenerateBoard ->
            generateBoard model

        GetSeed time ->
            CommonUpdate.getSeed model time

        MakeAIMove ->
            if GameModel.canMove model then
                randomAIMove model ! [ CommonPorts.sound "clack" ]
            else
                model ! []

        RecieveEditorFile value ->
            let
                newModel =
                    value
                        |> TransferModel.parse
                        |> Result.map (applyTransferModelToGameModel model)
                        |> Result.withDefault model
            in
                newModel ! []


generateBoard : Model -> ( Model, Cmd Msg )
generateBoard model =
    let
        ( spaces, pieces, gameResult, newSeed ) =
            generateBoardInfo model

        cmdList =
            if gameResult == GameModel.TBD then
                []
            else
                [ CommonPorts.alert "This game has ended before it began! You might want to adjust some parameters to make this less likely to happen again." ]
    in
        { model
            | seed = newSeed
            , spaces = spaces
            , pieces = pieces
            , gameResult = gameResult
        }
            ! cmdList


generateBoardInfo : Model -> ( Spaces, Pieces, GameResult, Seed )
generateBoardInfo model =
    generateBoardInfoHelper 4 model


generateBoardInfoHelper : Int -> Model -> ( Spaces, Pieces, GameResult, Seed )
generateBoardInfoHelper attempts model =
    let
        ( spaces, postSpacesSeed ) =
            Model.makeSpaces model.exportModel.gridWidth
                model.exportModel.gridHeight
                model.exportModel.spaceDeck
                model.seed

        ( pieces, newSeed ) =
            Model.makePieces spaces
                model.exportModel.pieceDeck
                postSpacesSeed

        gameResult =
            getGameResult model pieces
    in
        if gameResult == GameModel.TBD || attempts <= 0 then
            ( spaces, pieces, gameResult, newSeed )
        else
            generateBoardInfoHelper (attempts - 1) { model | seed = newSeed }


randomAIMove : Model -> Model
randomAIMove model =
    let
        moveList =
            Movement.getPossibleMoveList model
    in
        case Deck.maybeFromDeck moveList model.seed of
            Just ( ( pieceID, spaceID ), _, newSeed ) ->
                let
                    pieces =
                        Movement.getNewPieces model pieceID spaceID
                in
                    { model
                        | pieces = pieces
                        , seed = newSeed
                        , gameResult = getGameResult model pieces
                    }

            Nothing ->
                model


getGameResult : Model -> Pieces -> GameResult
getGameResult model pieces =
    case model.exportModel.gameEndCons of
        GameEndCons winCondition loseCondition ->
            if checkPredicate model winCondition pieces then
                GameModel.Win
            else if checkPredicate model loseCondition pieces then
                GameModel.Loss
            else
                GameModel.TBD


checkPredicate : Model -> GamePredicate -> Pieces -> Bool
checkPredicate model predicate pieces =
    case predicate of
        NoPiecesControlledBy controllability ->
            Pieces.controllabiltyCount controllability pieces <= 0

        NoPiecesStrictlyControlledBy controllability ->
            Pieces.strictControllabiltyCount controllability pieces <= 0

        NoPiecesOfGivenTypeCanMove piecetype ->
            let
                matchingPieceIndicies =
                    Dict.toList pieces
                        |> List.filterMap
                            (\( index, piece ) ->
                                if piece.pieceType == piecetype then
                                    Just index
                                else
                                    Nothing
                            )

                indexPairs =
                    List.concatMap
                        (\spaceIndex ->
                            List.map
                                (\pieceIndex ->
                                    ( pieceIndex, spaceIndex )
                                )
                                matchingPieceIndicies
                        )
                        <| Dict.keys model.spaces
            in
                not
                    <| List.any
                        (\( index, spaceIndex ) ->
                            Movement.canPieceMoveToSpace pieces
                                model.spaces
                                index
                                spaceIndex
                        )
                        indexPairs
