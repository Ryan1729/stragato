port module GamePorts exposing (..)

import Json.Encode as Encode

port recieveFromEditor :  (Encode.Value -> msg) -> Sub msg
