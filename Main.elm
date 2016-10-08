module Main exposing (..)

import Html.App exposing (program)
import View exposing (view)
import Model exposing (defaultState)
import Update exposing (update)
import Msg exposing (Msg(Animate, GetSeed, Resize, RecieveLoadedFile))
import Window
import Mouse
import AnimationFrame
import Time
import Task
import Ports


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
          , Task.perform (always <| Resize { width = 600, height = 600 }) Resize Window.size
          ]


alwaysList =
    [ Window.resizes Resize
    , Ports.recieveFile RecieveLoadedFile
      -- , AnimationFrame.diffs Animate
    ]


subscriptions model =
    Sub.batch <| alwaysList
