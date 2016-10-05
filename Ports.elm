port module Ports exposing (..)


port alert : String -> Cmd msg


port sound : String -> Cmd msg



-- (fileContents, filename)


port saveAs : ( String, String ) -> Cmd msg
