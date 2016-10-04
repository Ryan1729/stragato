port module Ports exposing (..)


port sound : String -> Cmd msg



-- (fileContents, filename)


port saveAs : ( String, String ) -> Cmd msg
