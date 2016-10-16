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
        [ cell [ size All 2 ]
            [ Button.render Msg.Mdl
                [ -7 ]
                model.mdl
                [ Button.onClick Msg.SaveAs
                , css "margin" "0 24px"
                ]
                [ Icon.i "file_download"
                , Options.span [ css "width" "4px" ] []
                , text "save"
                ]
            ]
        , cell [ size All 2 ]
            [ if model.showFileInput then
                Html.input
                    [ Html.Attributes.type' "file"
                    , Html.Attributes.id "fileInput"
                    ]
                    []
              else
                Button.render Msg.Mdl
                    [ -8 ]
                    model.mdl
                    [ Button.onClick Msg.Load
                    , css "margin" "0 24px"
                    ]
                    [ Icon.i "file_upload"
                    , Options.span [ css "width" "4px" ] []
                    , text "load"
                    ]
            ]
        , cell [ size All 2 ]
            [ Button.render Msg.Mdl
                [ -9 ]
                model.mdl
                [ Button.onClick Msg.Export
                , css "margin" "0 24px"
                ]
                [ Icon.i "play_for_work"
                , Options.span [ css "width" "4px" ] []
                , text "export"
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
                    [ text (toString model)
                    ]
        ]
    , Html.hr [] []
    , text "1.0.0"
    ]
