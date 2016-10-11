module GameView exposing (..)

import GameModel exposing (Model, GameResult(..))
import Html exposing (Html, div, text)
import Html.Attributes as HA
import Svg exposing (Svg, svg, rect, polygon, Attribute)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick, on)
import GameMsg exposing (Msg(..))
import Playfield
import Points


background =
    [ rect
        [ x "0"
        , y "0"
        , width "100%"
        , height "100%"
        , fill "#7F9Fff"
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


fontHeightFraction =
    0.05


view model =
    let
        viewWidth =
            playfieldWidth * model.exportModel.viewScale

        viewHeight =
            playfieldHeight * model.exportModel.viewScale

        gameResultText =
            case model.gameResult of
                TBD ->
                    []

                Win ->
                    [ makeBottomCenterText viewWidth viewHeight "You Win" ]

                Loss ->
                    [ makeBottomCenterText viewWidth viewHeight "You Lost" ]
    in
        div
            [ HA.style
                [ ( "display", "flex" )
                , ( "flex-direction", "column" )
                , ( "align-items", "center" )
                ]
            ]
            [ Html.button
                [ onClick GenerateBoard
                , HA.style
                    <| [ ( "font-size", "xx-large" )
                       , ( "padding", "0px" )
                       , ( "margin", "0px" )
                       , ( "width", px playfieldWidth )
                       , ( "height", px (playfieldHeight // 8) )
                       , ( "pointer-events", "auto" )
                       ]
                ]
                [ text "New GameResult" ]
            , svg
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
                ++ gameResultText
            , Html.button
                [ onClick MakeAIMove
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


makeBottomCenterText : Float -> Float -> String -> Svg Msg
makeBottomCenterText viewWidth viewHeight string =
    let
        fontHeightString =
            viewHeight
                * fontHeightFraction
                |> truncate
                |> toString
    in
        Svg.text'
            [ x <| toString (viewWidth / 2)
            , y <| toString (viewHeight * (1 - fontHeightFraction))
            , fontSize fontHeightString
            , textAnchor "middle"
            ]
            [ Svg.text string ]
