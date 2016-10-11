port module Ports exposing (..)


port alert : String -> Cmd msg


port sound : String -> Cmd msg


port load : () -> Cmd msg


port recieveFile : (String -> msg) -> Sub msg
