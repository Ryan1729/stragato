module Main exposing (..)

import Html.App exposing (program)
import View exposing (view)


main : Program Never
main =
    program
        { init = ( (), Cmd.none )
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



--type alias Model = {
--  width = 8,
--  height = 6,
--}


update : msg -> () -> ( (), Cmd msg )
update msg _ =
    () ! []


subscriptions =
    always Sub.none
