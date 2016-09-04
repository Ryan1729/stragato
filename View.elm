module View exposing (..)

import Html exposing (Html, div)
import Html.Attributes as HA
import Svg exposing (Svg, svg, rect, polygon, Attribute)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick, on)
import Msg exposing (Msg(PlayClack))
import Mouse
import Json.Decode
import DevControls
import Playfield
import Model exposing (Model)


background =
    [ rect
        [ x "0"
        , y "0"
        , width "100%"
        , height "100%"
        , fill "#08f"
        , onClick PlayClack
        , cursor "pointer"
        ]
        []
    ]


view : Model -> Html Msg
view model =
    let
        playfield =
            [ svg [ width "600", height "400", viewBox "0 0 600 400" ]
                <| background
                ++ Playfield.getSpaces model
                ++ Playfield.getPieces model
            ]

        elements =
            if model.debug then
                playfield
                    ++ DevControls.make model
            else
                playfield
    in
        (div [ HA.style [ ( "color", "#111" ) ] ]
            elements
        )
