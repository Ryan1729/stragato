port module EditorPorts exposing (..)

import Json.Encode as Encode


port load : () -> Cmd msg


port exportGame : Encode.Value -> Cmd msg


port recieveFile : (String -> msg) -> Sub msg


port sendToGame : Encode.Value -> Cmd msg
