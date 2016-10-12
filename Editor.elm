module Editor exposing (..)

import Html.App exposing (program)
import View exposing (view)
import Model exposing (defaultState)
import Update exposing (update)
import Msg exposing (Msg(Animate, GetSeed, RecieveLoadedFile))
import Window
import Mouse
import AnimationFrame
import Time
import Task
import EditorPorts


main : Program Never
main =
    program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view =
            view
            --TODO try (Html.lazy view) once we have perf concerns
        }


init =
    defaultState
        ! [ Task.perform (always <| GetSeed -1.0) GetSeed Time.now
          ]


alwaysList =
    [ EditorPorts.recieveFile RecieveLoadedFile
      -- , AnimationFrame.diffs Animate
    ]


subscriptions model =
    Sub.batch <| alwaysList
