module Update exposing (update)

import Model exposing (Model, GameResult)
import Msg exposing (Msg(..), ExportMsg(..))
import Ports
import Mouse
import Math.Vector2 as V2 exposing (Vec2, vec2)
import Random exposing (Seed)
import Extras
import Material
import Spaces exposing (Spaces, SpaceType(..), SpaceIndex)
import Pieces exposing (Pieces, Piece, PieceType)
import PiecesAndSpaces
import PieceAppearances
import Dict exposing (Dict)
import Deck
import Movement
import GameEndCons exposing (GameEndCons(..), GamePredicate(..))
import ExportModel exposing (ExportModel)
import String


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        HitTable ->
            { model | pieceSelected = Nothing } ! [ Ports.sound "tableHit" ]

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
                    ! [ Ports.sound "clack" ]

        GenerateBoard ->
            generateBoard model

        SelectTab tabIndex ->
            { model | tabIndex = tabIndex } ! []

        SelectPieceDeckTab tabIndex ->
            { model | pieceDeckTabIndex = tabIndex } ! []

        UpdateExportModel exportMsg ->
            { model | exportModel = updateExportModel exportMsg model.exportModel } ! []

        ToggleSpaceOutlines ->
            { model | showSpaceOutlines = not model.showSpaceOutlines } ! []

        ToggleAllowMovingAllPieces ->
            { model | allowMovingAllPieces = not model.allowMovingAllPieces } ! []

        ToggleIgnoreGameResult ->
            { model | ignoreGameResult = not model.ignoreGameResult } ! []

        MakeAIMove ->
            if Model.canMove model then
                randomAIMove model ! [ Ports.sound "clack" ]
            else
                model ! []

        SaveAs ->
            model
                ! [ Ports.saveAs
                        ( ExportModel.toString model.exportModel
                        , "editorState.txt"
                        )
                  ]

        Load ->
            { model | showFileInput = True } ! [ Ports.load () ]

        RecieveLoadedFile fileString ->
            case ExportModel.parse fileString of
                Ok newExportModel ->
                    generateBoard
                        { model
                            | exportModel = newExportModel
                            , showFileInput = False
                        }

                Err message ->
                    case ExportModel.parseDefaultingOnError fileString of
                        Ok newExportModelUsingDefaults ->
                            let
                                ( newModel, cmd ) =
                                    generateBoard
                                        { model
                                            | exportModel = newExportModelUsingDefaults
                                            , showFileInput = False
                                        }
                            in
                                newModel
                                    ! [ cmd
                                      , Ports.alert
                                            <| "Falling back to some defaults since parsing the file failed with this message: "
                                            ++ message
                                      ]

                        Err message2 ->
                            model ! [ Ports.alert message2 ]

        --TODO animate something or remove this!
        Animate _ ->
            model ! []

        Resize size ->
            { model | windowSize = size } ! []

        GetSeed time ->
            { model
                | seed =
                    Random.initialSeed
                        <| Debug.log "seed"
                        <| round time
            }
                ! []

        NoOp ->
            model ! [ Ports.sound "tableHit" ]

        -- When the `Mdl` messages come through, update appropriately.
        Mdl msg' ->
            Material.update msg'
                model


updateExportModel : ExportMsg -> ExportModel -> ExportModel
updateExportModel msg model =
    case msg of
        SpaceDeckIncrement item amount ->
            { model | spaceDeck = List.repeat amount item ++ model.spaceDeck }

        SpaceDeckDecrement item amount ->
            { model | spaceDeck = List.foldl (Extras.ignoreFirstArg <| Extras.remove item) model.spaceDeck [1..amount] }

        PieceDeckIncrement item amount ->
            { model | pieceDeck = List.repeat amount item ++ model.pieceDeck }

        PieceDeckDecrement item amount ->
            { model | pieceDeck = List.foldl (Extras.ignoreFirstArg <| Extras.remove item) model.pieceDeck [1..amount] }

        UpdateGridWidth newDimString ->
            case String.toInt newDimString of
                Ok dimension ->
                    { model | gridWidth = dimension }

                Err _ ->
                    model

        UpdateGridHeight newDimString ->
            case String.toInt newDimString of
                Ok dimension ->
                    { model | gridHeight = dimension }

                Err _ ->
                    model

        UpdateColour pieceType colourString ->
            { model
                | pieceAppearances =
                    PieceAppearances.updateColour pieceType
                        colourString
                        model.pieceAppearances
            }

        UpdateViewScale newScaleString ->
            case String.toFloat newScaleString of
                Ok newScale ->
                    { model | viewScale = newScale }

                Err _ ->
                    model

        DecrementWinCon ->
            { model | gameEndCons = GameEndCons.decrementWinCon model.gameEndCons }

        IncrementWinCon ->
            { model | gameEndCons = GameEndCons.incrementWinCon model.gameEndCons }

        DecrementLossCon ->
            { model | gameEndCons = GameEndCons.decrementLossCon model.gameEndCons }

        IncrementLossCon ->
            { model | gameEndCons = GameEndCons.incrementLossCon model.gameEndCons }

        DecrementSubWinCon ->
            { model | gameEndCons = Debug.log "" <| GameEndCons.decrementSubWinCon model.gameEndCons }

        IncrementSubWinCon ->
            { model | gameEndCons = GameEndCons.incrementSubWinCon model.gameEndCons }

        DecrementSubLossCon ->
            { model | gameEndCons = GameEndCons.decrementSubLossCon model.gameEndCons }

        IncrementSubLossCon ->
            { model | gameEndCons = GameEndCons.incrementSubLossCon model.gameEndCons }

        EditPoints pieceType newPoints ->
            { model
                | pieceAppearances =
                    PieceAppearances.updatePoints pieceType
                        newPoints
                        model.pieceAppearances
            }

        SetIcon icon pieceType ->
            { model
                | pieceAppearances =
                    PieceAppearances.updateIcon pieceType
                        icon
                        model.pieceAppearances
            }


generateBoard : Model -> ( Model, Cmd Msg )
generateBoard model =
    let
        ( spaces, pieces, gameResult, newSeed ) =
            generateBoardInfo model

        cmdList =
            if gameResult == Model.TBD then
                []
            else
                [ Ports.alert "This game has ended before it began! You might want to adjust some parameters to make this less likely to happen again." ]
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
        if gameResult == Model.TBD || attempts <= 0 then
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
                Model.Win
            else if checkPredicate model loseCondition pieces then
                Model.Loss
            else
                Model.TBD


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
