module View exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes as HA
import Svg exposing (Svg, svg, rect, polygon, Attribute)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick, on)
import Msg exposing (Msg(HitTable))
import Mouse
import Json.Decode
import DevControls
import Playfield
import Model exposing (Model)
import Points


background =
    [ rect
        [ x "0"
        , y "0"
        , width "100%"
        , height "100%"
        , fill "#08f"
        , onClick HitTable
        , cursor "pointer"
        ]
        []
    ]


playfieldWidth =
    600


playfieldHeight =
    400


playfieldWidthString =
    toString playfieldWidth


playfieldHeightString =
    toString playfieldHeight


px thing =
    (toString thing) ++ "px"


view : Model -> Html Msg
view model =
    let
        -- size = model.windowSize
        -- windowWidth = size.width
        -- windowHeight = size.height
        viewWidth =
            playfieldWidth * model.viewScale

        viewHeight =
            playfieldHeight * model.viewScale

        playfield =
            [ svg
                [ width playfieldWidthString
                , height playfieldHeightString
                , viewBox
                    <| "0 0 "
                    ++ toString viewWidth
                    ++ " "
                    ++ toString viewHeight
                ]
                <| background
                ++ Playfield.getSpaces model
                ++ Playfield.getPieces model
            , Html.button
                [ onClick Msg.ClearPieceSelection
                , HA.style
                    <| [ ( "font-size", "xx-large" )
                       , ( "padding", "0px" )
                       , ( "margin", "0px" )
                       , ( "width", px playfieldWidth )
                       , ( "height", px (playfieldHeight // 8) )
                       , ( "pointer-events", "auto" )
                       ]
                ]
                [ text "End turn" ]
            ]

        elements =
            if model.debug then
                playfield
                    ++ DevControls.make model
            else
                playfield
    in
        (div []
            elements
        )
