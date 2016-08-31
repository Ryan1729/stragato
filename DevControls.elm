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
import Material.Table as Table


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
                    , deckControl [ 2 ]
                        model.mdl
                        Model.spaceTypePossibilities
                        model.spaceDeck
                    ]

            _ ->
                div [] [ text (toString model) ]
        ]
    ]


deckControl : List Int -> Mdl -> List a -> List a -> Html Msg
deckControl index mdl possibilities currentDeck =
    Table.table []
        [ Table.thead []
            [ Table.tr []
                [ Table.th [{- Table.onClick Reorder -}]
                    [ text "Deck Element" ]
                , Table.th [] [ text "remove" ]
                , Table.th [ Table.numeric ] [ text "Quantity" ]
                , Table.th [] [ text "add" ]
                ]
            ]
        , Table.tbody []
            (possibilities
                |> List.map
                    (\item ->
                        Table.tr []
                            -- [ Table.td [] [ text "aaaaaaaaaaaaaaaaaaaaa" ]
                            -- , Table.td [] [ text "aaaaaaaaaaaaaaaaaaaaa" ]
                            -- , Table.td [] [ text "aaaaaaaaaaaaaaaaaaaaa" ]
                            -- , Table.td [] [ text "aaaaaaaaaaaaaaaaaaaaaa" ]
                            -- ]
                            [ Table.td []
                                [ text <| toString item
                                  --TODO factor out view function for each thing and pass it in
                                ]
                            , Table.td []
                                [ Button.render Msg.Mdl
                                    (index ++ [ 0 ])
                                    mdl
                                    [ Button.onClick Msg.GenerateBoard
                                      --TODO DeckDecrement
                                      -- , css "margin" "0 24px"
                                    ]
                                    [ Icon.i "remove"
                                      --  , text "Generate Board"
                                    ]
                                ]
                            , Table.td [] [ text <| toString <| amountOfItemInDeck item currentDeck ]
                            , Table.td []
                                [ Button.render Msg.Mdl
                                    (index ++ [ 1 ])
                                    mdl
                                    [ Button.onClick Msg.GenerateBoard
                                      --TODO DeckIncrement
                                      -- , css "margin" "0 24px"
                                    ]
                                    [ Icon.i "add"
                                      --  , text "Generate Board"
                                    ]
                                ]
                            ]
                    )
            )
        ]


amountOfItemInDeck : a -> List a -> Int
amountOfItemInDeck item deck =
    List.filter ((==) item) deck
        |> List.length
