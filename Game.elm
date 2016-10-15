module Game exposing (..)

import Html.App exposing (programWithFlags)
import GameModel
import GameView exposing (view)
import GameUpdate exposing (update)
import GamePorts
import Time
import GameMsg exposing (Msg(GetSeed, RecieveEditorFile))
import Task
import GameModel exposing (Model, applyTransferModelToGameModel, applyExportModelToGameModel)
import TransferModel
import ExportModel
import Dict
import Json.Decode as Decode


main : Program (Maybe Decode.Value)
main =
    programWithFlags
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view =
            view
            --TODO try (Html.lazy view) once we have perf concerns
        }


bugWorkaround =
    GameModel.defaultState


init : Maybe Decode.Value -> ( Model, Cmd Msg )
init maybeValue =
    let
        resultValue =
            Result.fromMaybe "" maybeValue

        state =
            case resultValue `Result.andThen` TransferModel.parse of
                Ok transferModel ->
                    transferModel
                        |> applyTransferModelToGameModel GameModel.defaultState

                Err _ ->
                    case resultValue `Result.andThen` ExportModel.parse of
                        Ok exportModel ->
                            applyExportModelToGameModel GameModel.defaultState exportModel

                        Err _ ->
                            GameModel.defaultState
    in
        state
            ! [ Task.perform (always <| GetSeed -1.0) GetSeed Time.now
              ]


alwaysList =
    [ GamePorts.recieveFromEditor RecieveEditorFile
      -- , AnimationFrame.diffs Animate
    ]


subscriptions model =
    Sub.batch <| alwaysList
