module View exposing (..)

import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)


view : () -> Html msg
view _ =
    svg [ width "600", height "600", viewBox "0 0 1024 1024" ]
        [ rect
            [ x "0"
            , y "0"
            , width "100%"
            , height "100%"
            , fill "#08f"
            ]
            []
        ]
