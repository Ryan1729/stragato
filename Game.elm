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


-- import TransferModel exposing (TransferModel)

import Dict


-- main : Program  (Maybe TransferModel)


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



-- init : Maybe TransportModel -> (Model, Cmd Msg)


init : Maybe Int -> ( Model, Cmd Msg )
init maybeTransportModel =
    let
        state =
            case maybeTransportModel of
                -- Just transportModel ->
                --     { bugWorkaround
                --         | exportModel = transportModel.exportModel
                --         , pieces = transportModel.pieces
                --         , spaces = Dict.fromList transportModel.spaces
                --         , ignoreGameResult = transportModel.ignoreGameResult
                --         , showSpaceOutlines = transportModel.showSpaceOutlines
                --         , allowMovingAllPieces = transportModel.allowMovingAllPieces
                --     }
                --
                -- Nothing ->
                _ ->
                    GameModel.defaultState
    in
        state
            ! [ Task.perform (always <| GetSeed -1.0) GetSeed Time.now
              ]


alwaysList =
    [-- , AnimationFrame.diffs Animate
    ]


subscriptions model =
    Sub.batch <| alwaysList
