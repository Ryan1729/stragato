module Main exposing (..)

import Html.App exposing (program)
import View exposing (view)
import Model exposing (init)
import Update exposing (update)
import Msg exposing (Msg(PieceDragMove, PieceDragEnd))
import Mouse


main : Program Never
main =
    program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


subscriptions model =
    case model.pieceDrag of
        Nothing ->
            Sub.none

        Just _ ->
            Sub.batch
                [ Mouse.moves PieceDragMove
                , Mouse.ups PieceDragEnd
                ]
