module Game exposing (..)

import Html.App exposing (programWithFlags)
import GameModel
import GameView exposing (view)
import GameUpdate exposing (update)
import CommonPorts
import Time
import GameMsg exposing (Msg(GetSeed))
import Task
import GameModel exposing (Model)
import TransferModel exposing (TransferModel)
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
        state =
            Result.fromMaybe ""
                maybeValue
                `Result.andThen` TransferModel.parse
                |> Result.map (applyTransferModelToGameModel GameModel.defaultState)
                |> Result.withDefault GameModel.defaultState
    in
        state
            ! [ Task.perform (always <| GetSeed -1.0) GetSeed Time.now
              ]


applyTransferModelToGameModel : Model -> TransferModel -> Model
applyTransferModelToGameModel gameModel transferModel =
    { gameModel
        | exportModel = transferModel.exportModel
        , pieces = transferModel.pieces
        , spaces = transferModel.spaces
        , ignoreGameResult = transferModel.ignoreGameResult
        , showSpaceOutlines = transferModel.showSpaceOutlines
        , allowMovingAllPieces = transferModel.allowMovingAllPieces
    }


alwaysList =
    [-- , AnimationFrame.diffs Animate
    ]


subscriptions model =
    Sub.batch <| alwaysList
