module DevControls exposing (make)

import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Model exposing (Model)
import Msg exposing (Msg)
import Material
import Material.Scheme
import Material.Options as Options exposing (css)
import Material.Button as Button
import Material.Icon as Icon
import Material.Tabs as Tabs
import Material.Table as Table
import Math.Vector2 as V2 exposing (Vec2, vec2)
import Svg exposing (Svg, svg)
import Svg.Attributes exposing (height, width, viewBox)
import Playfield
import PlayfieldComponents


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
                        PlayfieldComponents.spaceTypePossibilities
                        model.spaceDeck
                        Msg.SpaceDeckDecrement
                        Msg.SpaceDeckIncrement
                        (positionedSvgMakerToHtmlMaker <| Playfield.space [])
                    ]

            _ ->
                div [] [ text (toString model) ]
        ]
    ]


positionedSvgMakerToHtmlMaker : (Vec2 -> a -> Svg Msg) -> a -> Html Msg
positionedSvgMakerToHtmlMaker svgMaker identifier =
    svg [ width "50", height "50", viewBox "0 0 150 150" ]
        [ svgMaker (vec2 75 75) identifier ]


tup =
    (,)


deckControl :
    List Int
    -> Mdl
    -> List a
    -> List a
    -> (a -> Msg)
    -> (a -> Msg)
    -> (a -> Html Msg)
    -> Html Msg
deckControl index mdl possibilities currentDeck addMessage removeMessage elementView =
    Table.table [ css "background-color" "#DDDDDD" ]
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
                            [ Table.td []
                                [ elementView item
                                ]
                            , Table.td []
                                [ Button.render Msg.Mdl
                                    (index ++ [ 0 ])
                                    mdl
                                    [ Button.onClick <| addMessage item
                                    ]
                                    [ Icon.i "remove"
                                    ]
                                ]
                            , Table.td []
                                [ text
                                    <| toString
                                    <| amountOfItemInDeck item currentDeck
                                ]
                            , Table.td []
                                [ Button.render Msg.Mdl
                                    (index ++ [ 1 ])
                                    mdl
                                    [ Button.onClick <| removeMessage item
                                    ]
                                    [ Icon.i "add"
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
