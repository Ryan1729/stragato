module DevControls exposing (make)

import Html exposing (Html, div, text)
import Model exposing (Model)
import Msg exposing (Msg)
import Material
import Material.Scheme
import Material.Options as Options exposing (css)
import Material.Button as Button
import Material.Icon as Icon
import Material.Tabs as Tabs


type alias Mdl =
    Material.Model


make : Model -> List (Html Msg)
make model =
    [ Html.hr [] []
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
            , text "reset"
            ]
        , Tabs.label [ Options.center ]
            [ Icon.i "info_outline"
            , Options.span [ css "width" "4px" ] []
            , text "model"
            ]
        ]
        [ case model.tabIndex of
            0 ->
                div []
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

            _ ->
                div [] [ text (toString model) ]
        ]
    ]
