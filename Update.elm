module Update exposing (update)

import Model exposing (Model)
import Msg exposing (Msg(..), ExportMsg(..))
import EditorPorts
import CommonPorts
import Math.Vector2 as V2 exposing (Vec2, vec2)
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
import CommonUpdate
import TransferModel


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        SelectTab tabIndex ->
            { model | tabIndex = tabIndex } ! []

        SelectPieceDeckTab tabIndex ->
            { model | pieceDeckTabIndex = tabIndex } ! []

        UpdateExportModel exportMsg ->
            let
                newModel =
                    { model | exportModel = updateExportModel exportMsg model.exportModel }
            in
                newModel ! [ newModel |> encodeAndSend ]

        ToggleSpaceOutlines ->
            let
                newModel =
                    { model | showSpaceOutlines = not model.showSpaceOutlines }
            in
                newModel ! [ newModel |> encodeAndSend ]

        ToggleAllowMovingAllPieces ->
            let
                newModel =
                    { model | allowMovingAllPieces = not model.allowMovingAllPieces }
            in
                newModel ! [ newModel |> encodeAndSend ]

        ToggleIgnoreGameResult ->
            let
                newModel =
                    { model | ignoreGameResult = not model.ignoreGameResult }
            in
                newModel ! [ newModel |> encodeAndSend ]

        SaveAs ->
            model
                ! [ CommonPorts.saveAs
                        ( ExportModel.toString model.exportModel
                        , "editorState.txt"
                        )
                  ]

        Load ->
            { model | showFileInput = True } ! [ EditorPorts.load () ]

        Export ->
            model ! [ model.exportModel |> ExportModel.encode |> EditorPorts.exportGame ]

        RecieveLoadedFile fileString ->
            case ExportModel.parseString fileString of
                Ok newExportModel ->
                    let
                        newModel =
                            { model
                                | exportModel = newExportModel
                                , showFileInput = False
                            }
                    in
                        newModel ! [ newModel |> encodeAndSend ]

                Err message ->
                    case ExportModel.parseStringDefaultingOnError fileString of
                        Ok newExportModelUsingDefaults ->
                            let
                                newModel =
                                    { model
                                        | exportModel = newExportModelUsingDefaults
                                        , showFileInput = False
                                    }
                            in
                                newModel
                                    ! [ CommonPorts.alert
                                            <| "Falling back to some defaults since parsing the file failed with this message: "
                                            ++ message
                                      , newModel |> encodeAndSend
                                      ]

                        Err message2 ->
                            model
                                ! [ CommonPorts.alert message2
                                  ]

        --TODO animate something or remove this!
        Animate _ ->
            model ! []

        GetSeed time ->
            CommonUpdate.getSeed model time

        NoOp ->
            model ! [ CommonPorts.sound "tableHit" ]

        -- When the `Mdl` messages come through, update appropriately.
        Mdl msg' ->
            Material.update msg'
                model


encodeAndSend : Model -> Cmd Msg
encodeAndSend =
    Model.modelToTransferModelWithoutPieces
        >> TransferModel.encode
        >> EditorPorts.sendToGame


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
            { model | gameEndCons = GameEndCons.decrementSubWinCon model.gameEndCons }

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
