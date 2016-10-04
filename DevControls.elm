module DevControls exposing (make)

import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onInput)
import Model exposing (Model)
import Msg exposing (Msg(..))
import Material.Button as Button
import Material.Grid exposing (grid, cell, size, offset, Device(All, Tablet))
import Material.Icon as Icon
import Material.Options as Options exposing (css)
import Material.Tabs as Tabs
import PieceAppearances exposing (PieceAppearances)
import AppearanceTab
import EditTab


make : Model -> List (Html Msg)
make model =
    [ Html.hr [] []
    , grid []
        [ cell [ size All 4 ]
            [ Button.render Msg.Mdl
                [ 1 ]
                model.mdl
                [ Button.onClick Msg.GenerateBoard
                , css "margin" "0 24px"
                ]
                [ Icon.i "cached"
                , Options.span [ css "width" "4px" ] []
                , text "Generate Board"
                ]
            ]
        , cell [ size All 4 ]
            [ Button.render Msg.Mdl
                [ -7 ]
                model.mdl
                [ Button.onClick Msg.SaveAs
                , css "margin" "0 24px"
                ]
                [ Icon.i "save"
                , Options.span [ css "width" "4px" ] []
                , text "save"
                ]
            ]
        ]
    , Tabs.render Msg.Mdl
        [ -1 ]
        model.mdl
        [ Tabs.ripple
        , Tabs.onSelectTab Msg.SelectTab
        , Tabs.activeTab model.tabIndex
        ]
        [ Tabs.label [ Options.center ]
            [ Icon.i "edit"
            , Options.span [ css "width" "4px" ] []
            , text "edit rules"
            ]
        , Tabs.label [ Options.center ]
            [ Icon.i "palette"
            , Options.span [ css "width" "4px" ] []
            , text "edit appearance"
            ]
        , Tabs.label [ Options.center ]
            [ Icon.i "info_outline"
            , Options.span [ css "width" "4px" ] []
            , text "model"
            ]
        ]
        [ case model.tabIndex of
            0 ->
                EditTab.render model

            1 ->
                AppearanceTab.render model

            _ ->
                div []
                    [ text <| toString model.gameResult
                    , Html.hr [] []
                    , text (toString model)
                    ]
        ]
    ]
