module Main exposing (..)

import Html.App exposing (program)
import Msg exposing (Msg(..))
import View exposing (view)
import Ports


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


update : Msg -> () -> ( (), Cmd msg )
update message _ =
    case message of
        PlayClack ->
            () ! [ Ports.sound "clack" ]


subscriptions =
    always Sub.none
