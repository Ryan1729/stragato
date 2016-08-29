module Main exposing (..)

import Html.App exposing (program)
import View exposing (view)
import Model exposing (defaultState)
import Update exposing (update)
import Msg exposing (Msg(Animate, GetSeed))
import Mouse
import AnimationFrame
import Time
import Task


main : Program Never
main =
    program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init =
    defaultState ! [ Task.perform (always <| GetSeed -1.0) GetSeed Time.now ]


alwaysList =
    [ AnimationFrame.diffs Animate ]


subscriptions model =
    let
        sometimesList =
            []
    in
        Sub.batch <| alwaysList ++ sometimesList
