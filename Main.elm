module Main exposing (..)

import Html.App exposing (program)
import View exposing (view)
import Model exposing (init)
import Update exposing (update)
import Msg exposing (Msg(Animate))
import Mouse
import AnimationFrame


main : Program Never
main =
    program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


alwaysList =
    [ AnimationFrame.diffs Animate ]


subscriptions model =
    let
        sometimesList =
            []
    in
        Sub.batch <| alwaysList ++ sometimesList
