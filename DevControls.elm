module DevControls exposing (make)

import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Model exposing (Model)
import Msg exposing (Msg(..))
import Material
import Material.Scheme
import Material.Options as Options exposing (css)
import Material.Button as Button
import Material.Icon as Icon
import Material.Slider as Slider
import Material.Tabs as Tabs
import Material.Table as Table
import Material.Toggles as Toggles
import Material.Grid exposing (grid, cell, size, offset, Device(All, Tablet))
import Math.Vector2 as V2 exposing (Vec2, vec2)
import Svg exposing (Svg, svg)
import Svg.Attributes exposing (height, width, viewBox, stroke)
import Playfield
import Pieces
import Spaces


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
            , text "edit"
            ]
        , Tabs.label [ Options.center ]
            [ Icon.i "info_outline"
            , Options.span [ css "width" "4px" ] []
            , text "model"
            ]
        ]
        [ case model.tabIndex of
            0 ->
                editTab model

            _ ->
                div [] [ text <| toString model.gameResult, Html.hr [] [], text (toString model) ]
        ]
    ]


editTab : Model -> Html Msg
editTab model =
    grid []
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
        , toggleSwitchCell [ 8 ]
            model.mdl
            Msg.ToggleSpaceOutlines
            "Show outlines of empty spaces"
            model.showSpaceOutlines
        , toggleSwitchCell [ 9 ]
            model.mdl
            Msg.ToggleSelfMoves
            "Allow \"moving\" to same space"
            model.allowSelfMoves
        , toggleSwitchCell [ 10 ]
            model.mdl
            Msg.ToggleAllowMovingAllPieces
            "Allow moving all pieces"
            model.allowMovingAllPieces
        , toggleSwitchCell [ 11 ]
            model.mdl
            Msg.ToggleIgnoreGameResult
            "Allow moving pieces after end of game"
            model.ignoreGameResult
          -- , cell [ size All 4 ] []
        , cell [ size All 4 ]
            <| makeStepper [ 12 ]
                model.mdl
                "win condition"
                DecrementWinCon
                IncrementWinCon
                (Model.getWinConString model)
        , cell [ size All 4 ]
            <| makeStepper [ 13 ]
                model.mdl
                "loss condition"
                DecrementLossCon
                IncrementLossCon
                (Model.getLossConString model)
        , cell [ size All 4 ]
            <| makeStepper [ 5 ]
                model.mdl
                "gridWidth"
                DecrementGridWidth
                IncrementGridWidth
                (toString model.gridWidth)
        , cell [ size All 4 ]
            <| makeStepper [ 6 ]
                model.mdl
                "gridHeight"
                DecrementGridHeight
                IncrementGridHeight
                (toString model.gridHeight)
        , cell [ size All 4 ]
            <| makeStepper [ 7 ]
                model.mdl
                "scale"
                DecrementViewScale
                IncrementViewScale
                (toString model.viewScale)
        , cell [ size All 6 ]
            [ deckControl [ 2 ]
                model.mdl
                Spaces.spaceTypePossibilities
                model.spaceDeck
                Msg.SpaceDeckDecrement
                Msg.SpaceDeckIncrement
                (positionedSvgMakerToHtmlMaker
                    <| Playfield.space model.showSpaceOutlines [ stroke "grey" ]
                )
            ]
        , cell [ size All 6 ]
            [ deckControl [ 3 ]
                model.mdl
                Pieces.pieceTypePossibilities
                model.pieceDeck
                Msg.PieceDeckDecrement
                Msg.PieceDeckIncrement
                (positionedSvgMakerToHtmlMaker <| Playfield.piece [ stroke "grey" ])
            ]
        ]


toggleSwitchCell : List Int -> Mdl -> Msg -> String -> Bool -> Material.Grid.Cell Msg
toggleSwitchCell index mdl toggleMessage labelText bool =
    cell [ size All 4 ]
        [ Toggles.switch Mdl
            index
            mdl
            [ Toggles.onClick toggleMessage
            , Toggles.ripple
            , Toggles.value bool
            ]
            [ text labelText ]
        ]


makeStepper : List Int -> Mdl -> String -> Msg -> Msg -> String -> List (Html Msg)
makeStepper index mdl label decrementMsg incrementMsg value =
    [ text label
    , div [ style [ ( "border", "1px solid" ) ] ]
        [ Button.render Msg.Mdl
            (index ++ [ 0 ])
            mdl
            [ Button.onClick decrementMsg
            ]
            [ Icon.i "remove"
            ]
        , text value
        , Button.render Msg.Mdl
            (index ++ [ 1 ])
            mdl
            [ Button.onClick incrementMsg
            ]
            [ Icon.i "add"
            ]
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
