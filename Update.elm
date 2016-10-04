module Update exposing (update)

import Model exposing (Model, GamePredicate(..), GameEndCons(..), GameResult, ExportModel)
import Msg exposing (Msg(..), ExportMsg(..))
import Ports
import Mouse
import Math.Vector2 as V2 exposing (Vec2, vec2)
import Random
import Extras
import Material
import Spaces exposing (Spaces, SpaceType(..), SpaceIndex)
import Pieces exposing (Pieces, Piece, PieceType)
import PiecesAndSpaces
import PieceAppearances
import Dict exposing (Dict)
import Deck
import Movement


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
            let
                ( spaces, postSpacesSeed ) =
                    Model.makeSpaces model.exportModel.gridWidth
                        model.exportModel.gridHeight
                        model.exportModel.spaceDeck
                        model.seed

                ( pieces, newSeed ) =
                    Model.makePieces spaces
                        model.exportModel.pieceDeck
                        model.exportModel.moveTypeDeck
                        postSpacesSeed
            in
                { model
                    | seed = newSeed
                    , spaces = spaces
                    , pieces = pieces
                    , gameResult = getGameResult model pieces
                }
                    ! []

        SelectTab tabIndex ->
            { model | tabIndex = tabIndex } ! []

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
            let
                contents =
                    "test"
            in
                model ! [ Ports.saveAs ( contents, "editorState.txt" ) ]

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

        IncrementGridWidth ->
            { model | gridWidth = model.gridWidth + 1 }

        DecrementGridWidth ->
            { model | gridWidth = max 0 (model.gridWidth - 1) }

        IncrementGridHeight ->
            { model | gridHeight = model.gridHeight + 1 }

        DecrementGridHeight ->
            { model | gridHeight = max 0 (model.gridHeight - 1) }

        UpdateColour pieceType colourString ->
            { model
                | pieceAppearances =
                    PieceAppearances.updateColour pieceType
                        colourString
                        model.pieceAppearances
            }

        IncrementViewScale ->
            { model | viewScale = higherScale model.viewScale }

        DecrementViewScale ->
            { model | viewScale = lowerScale model.viewScale }

        DecrementWinCon ->
            { model | gameEndCons = Model.decrementWinCon model.gameEndCons }

        IncrementWinCon ->
            { model | gameEndCons = Model.incrementWinCon model.gameEndCons }

        DecrementLossCon ->
            { model | gameEndCons = Model.decrementLossCon model.gameEndCons }

        IncrementLossCon ->
            { model | gameEndCons = Model.incrementLossCon model.gameEndCons }

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


higherScale : Float -> Float
higherScale oldScale =
    oldScale + 0.5


lowerScale : Float -> Float
lowerScale oldScale =
    if oldScale > 1 then
        oldScale - 0.5
    else
        oldScale
