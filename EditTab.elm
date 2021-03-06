module EditTab exposing (..)

import Html exposing (Html, Attribute, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onInput)
import Model exposing (Model)
import Msg exposing (Msg(..), ExportMsg(..))
import Material
import Material.Options as Options exposing (css)
import Material.Button as Button
import Material.Icon as Icon
import Material.List as Lists
import Material.Table as Table
import Material.Tabs as Tabs
import Material.Toggles as Toggles
import Material.Grid exposing (grid, cell, size, offset, Device(All, Tablet))
import Math.Vector2 as V2 exposing (Vec2, vec2)
import Svg exposing (Svg, svg)
import Svg.Attributes exposing (height, width, viewBox, stroke)
import Playfield
import Pieces exposing (Piece, PieceType, ProtoPiece(..), Controller(..), MoveOccupancy(..), Shape(..))
import Spaces
import PieceAppearances exposing (PieceAppearances)
import String
import DevControlsCommon as DCC
import GameEndCons
import Regex
import Extras
import QuantityControl exposing (QuantityControl)


render : Model -> Html Msg
render model =
    div []
        [ grid []
            [ toggleSwitchCell [ 8 ]
                model.mdl
                Msg.ToggleSpaceOutlines
                "Show outlines of empty spaces"
                model.showSpaceOutlines
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
            ]
        , grid []
            [ cell [ size All 6 ]
                <| makeGamePredicateSelector [ 12 ]
                    model.mdl
                    "win condition"
                    DecrementWinCon
                    IncrementWinCon
                    DecrementSubWinCon
                    IncrementSubWinCon
                    (GameEndCons.getWinConString model.exportModel.gameEndCons)
            , cell [ size All 6 ]
                <| makeGamePredicateSelector [ 13 ]
                    model.mdl
                    "loss condition"
                    DecrementLossCon
                    IncrementLossCon
                    DecrementSubLossCon
                    IncrementSubLossCon
                    (GameEndCons.getLossConString model.exportModel.gameEndCons)
            , cell [ size All 4 ]
                <| makeLabeledInput [ 5 ]
                    model.mdl
                    "gridWidth"
                    [ Html.Attributes.type' "number"
                    , Html.Attributes.min "1"
                    , Html.Attributes.step "1"
                    , style [ ( "flex", "1" ), ( "background-color", DCC.background ) ]
                    ]
                    (UpdateExportModel << UpdateGridWidth)
                    (toString model.exportModel.gridWidth)
            , cell [ size All 4 ]
                <| makeLabeledInput [ 6 ]
                    model.mdl
                    "gridHeight"
                    [ Html.Attributes.type' "number"
                    , Html.Attributes.min "1"
                    , Html.Attributes.step "1"
                    , style [ ( "flex", "1" ), ( "background-color", DCC.background ) ]
                    ]
                    (UpdateExportModel << UpdateGridHeight)
                    (toString model.exportModel.gridHeight)
            , cell [ size All 4 ]
                <| makeLabeledInput [ 7 ]
                    model.mdl
                    "scale"
                    [ Html.Attributes.type' "number"
                    , Html.Attributes.min "1"
                    , Html.Attributes.step "0.5"
                    , style [ ( "flex", "1" ), ( "background-color", DCC.background ) ]
                    ]
                    (UpdateExportModel << UpdateViewScale)
                    (toString model.exportModel.viewScale)
            ]
        , grid []
            [ cell [ size All 6 ]
                [ let
                    quantityControl =
                        QuantityControl.standard (UpdateExportModel ... Msg.SpaceDeckDecrement)
                            (UpdateExportModel ... Msg.SpaceDeckIncrement)
                            (\_ _ -> NoOp)
                  in
                    deckControl [ 2 ]
                        model.mdl
                        model.exportModel.spaceDeck
                        Spaces.spaceTypePossibilities
                        "space type"
                        DCC.displaySpaceType
                        quantityControl
                        (DCC.positionedSvgMakerToHtmlMaker
                            <| Playfield.space model.showSpaceOutlines [ stroke "grey" ]
                        )
                ]
            , cell [ size All 6 ]
                [ let
                    columnData =
                        List.map2 (,)
                            (splitOnController model.exportModel.pieceDeck)
                            pieceDeckContolTabLabels
                  in
                    div []
                        [ tabbedQuantityControlTable [ 3 ]
                            model.mdl
                            columnData
                            Msg.SelectPieceDeckTab
                            (protoPieceTabView model)
                            model.pieceDeckTabIndex
                        ]
                ]
            ]
        ]


splitOnController : List ProtoPiece -> List (List ProtoPiece)
splitOnController list =
    [ List.filter (protoControllerSplitter Pieces.isPlayerController) list
    , List.filter (protoControllerSplitter Pieces.isComputerController) list
    , List.filter (protoControllerSplitter Pieces.isBothController) list
    , List.filter ((==) NoPiece) list
        ++ List.filter (protoControllerSplitter Pieces.isNoneController) list
    ]


protoControllerSplitter : (Controller -> Bool) -> ProtoPiece -> Bool
protoControllerSplitter f protoPiece =
    case protoPiece of
        ActualPiece pieceType ->
            f pieceType.controller

        NoPiece ->
            False


protoPieceTabView :
    Model
    -> List Int
    -> Material.Model
    -> List ProtoPiece
    -> Html Msg
protoPieceTabView model index mdl dataList =
    let
        quantityControl =
            QuantityControl.confirmRemoval (UpdateExportModel ... Msg.PieceDeckDecrement)
                (UpdateExportModel ... Msg.PieceDeckIncrement)
                (\_ _ -> NoOp)

        table =
            quantityControlTable "piece type"
                displayProtoPieceType
                quantityControl
                (DCC.positionedSvgMakerToHtmlMaker
                    <| protoPieceToSVG model.exportModel.pieceAppearances
                )
                index
                mdl
                dataList

        newIndex =
            index ++ [ 2 ]

        editPanel =
            if model.showPieceEditor then
                makePieceTypeSelector newIndex mdl model.editingPieceType
            else
                Button.render Msg.Mdl
                    newIndex
                    mdl
                    [ Button.raised
                    , Button.ripple
                    , css "width" "100%"
                    , Button.onClick TogglePieceEditor
                    ]
                    [ Icon.i "add"
                    ]
    in
        wrapWithTotal (List.length dataList)
            <| [ table
               , editPanel
               ]


displayProtoPieceType : ProtoPiece -> List (Html Msg)
displayProtoPieceType protoPiece =
    case protoPiece of
        ActualPiece pieceType ->
            DCC.displayPiecetype pieceType

        NoPiece ->
            [ text "No piece" ]


protoPieceToSVG : PieceAppearances -> Vec2 -> ProtoPiece -> Svg Msg
protoPieceToSVG pieceAppearances center protoPiece =
    case protoPiece of
        ActualPiece pieceType ->
            DCC.pieceTypeToSVG pieceAppearances center pieceType

        NoPiece ->
            Playfield.nullSVG


toggleSwitchCell : List Int -> Material.Model -> Msg -> String -> Bool -> Material.Grid.Cell Msg
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


makePieceTypeSelector :
    List Int
    -> Material.Model
       -- -> ExportMsg -> ExportMsg -> ExportMsg -> ExportMsg
    -> PieceType
    -> Html Msg
makePieceTypeSelector index
    mdl
    -- decrementMsg incrementMsg decrementSubPredicateMsg incrementSubPredicateMsg
    pieceType
    =
    div
        [ style
            [ ( "border", "1px solid" )
            , ( "display", "flex" )
            , ( "flex-direction", "column" )
            ]
        ]
        [ mdlStepper index mdl NoOp NoOp (toString pieceType.moveEffect)
        , mdlStepper index mdl NoOp NoOp (toString pieceType.controller)
        , text <| toString pieceType.movePattern
        ]


mdlStepper index mdl incrementMsg decrementMsg value =
    Html.ul [ style [ ( "list-style", "none" ), ( "padding", "0" ), ( "margin", "0" ) ] ]
        [ Html.li []
            [ Button.render Msg.Mdl
                (index ++ [ 1 ])
                mdl
                [ Button.onClick incrementMsg
                ]
                [ Icon.i "add"
                ]
            ]
        , Html.li [ style [ ( "margin-left", "2em" ) ] ] [ text value ]
        , Html.li []
            [ Button.render Msg.Mdl
                (index ++ [ 0 ])
                mdl
                [ Button.onClick decrementMsg
                ]
                [ Icon.i "remove"
                ]
            ]
        ]


makeGamePredicateSelector : List Int -> Material.Model -> String -> ExportMsg -> ExportMsg -> ExportMsg -> ExportMsg -> String -> List (Html Msg)
makeGamePredicateSelector index mdl label decrementMsg incrementMsg decrementSubPredicateMsg incrementSubPredicateMsg value =
    let
        ( leftSubValue, rightSubValue ) =
            value
                |> Regex.split (Regex.AtMost 1) (Regex.regex " ")
                |> (\list ->
                        case list of
                            first :: (second :: tail) ->
                                ( first, second )

                            _ ->
                                ( "😯", "wat" )
                   )
    in
        [ text label
        , div
            [ style
                [ ( "border", "1px solid" )
                , ( "display", "flex" )
                ]
            ]
            [ Html.ul [ style [ ( "list-style", "none" ), ( "padding", "0" ), ( "margin", "0" ) ] ]
                [ Html.li []
                    [ Button.render Msg.Mdl
                        (index ++ [ 1 ])
                        mdl
                        [ Button.onClick (UpdateExportModel incrementMsg)
                        ]
                        [ Icon.i "add"
                        ]
                    ]
                , Html.li [ style [ ( "margin-left", "2em" ) ] ] [ text leftSubValue ]
                , Html.li []
                    [ Button.render Msg.Mdl
                        (index ++ [ 0 ])
                        mdl
                        [ Button.onClick (UpdateExportModel decrementMsg)
                        ]
                        [ Icon.i "remove"
                        ]
                    ]
                ]
            , Html.ul [ style [ ( "list-style", "none" ), ( "padding", "0" ), ( "margin", "0" ) ] ]
                [ Html.li []
                    [ Button.render Msg.Mdl
                        (index ++ [ 2 ])
                        mdl
                        [ Button.onClick (UpdateExportModel incrementSubPredicateMsg)
                        ]
                        [ Icon.i "add"
                        ]
                    ]
                , Html.li [ style [ ( "margin-left", "2em" ) ] ] [ text rightSubValue ]
                , Html.li []
                    [ Button.render Msg.Mdl
                        (index ++ [ -1 ])
                        mdl
                        [ Button.onClick (UpdateExportModel decrementSubPredicateMsg)
                        ]
                        [ Icon.i "remove"
                        ]
                    ]
                ]
            ]
        ]


makeLabeledInput : List Int -> Material.Model -> String -> List (Attribute Msg) -> (String -> Msg) -> String -> List (Html Msg)
makeLabeledInput index mdl label extraAttributes updateMsg value =
    [ text label
    , div [ style [ ( "display", "flex" ), ( "border", "1px solid" ) ] ]
        [ Html.input
            ([ onInput updateMsg
             , Html.Attributes.value value
             ]
                ++ extraAttributes
            )
            []
        ]
    ]


deckControl :
    List Int
    -> Material.Model
    -> List a
    -> List a
    -> String
    -> (a -> List (Html Msg))
    -> QuantityControl a Msg
    -> (a -> Html Msg)
    -> Html Msg
deckControl index mdl currentDeck possibilities typeHeading typeDisplay quantityControl elementView =
    wrapWithTotal (List.length currentDeck)
        <| [ Table.table [ css "background-color" DCC.background ]
                [ Table.thead []
                    [ Table.tr []
                        [ Table.th [{- Table.onClick Reorder -}]
                            [ text "Deck Element" ]
                        , Table.th [] [ text typeHeading ]
                        , Table.th [ Table.numeric ] [ text "Quantity" ]
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
                                        <| typeDisplay item
                                    , Table.td []
                                        [ quantityControl currentDeck item
                                        ]
                                    ]
                            )
                    )
                ]
           ]



-- the blackbird combinator
-- see https://youtu.be/seVSlKazsNk?t=11m53s


(...) : (a -> b) -> (c -> d -> a) -> c -> d -> b
(...) =
    (<<) << (<<)


tabbedQuantityControlTable :
    List Int
    -> Material.Model
    -> List ( List a, Tabs.Label Msg )
    -> (Int -> Msg)
    -> (List Int -> Material.Model -> List a -> Html Msg)
    -> Int
    -> Html Msg
tabbedQuantityControlTable index mdl bundleList selectTabMsg tabDisplay tabIndex =
    let
        tabLabels =
            List.map snd bundleList
    in
        Tabs.render Msg.Mdl
            (index ++ [ 20 ])
            mdl
            [ Tabs.ripple
            , Tabs.onSelectTab selectTabMsg
            , Tabs.activeTab tabIndex
            ]
            tabLabels
            [ List.drop tabIndex bundleList
                |> List.head
                |> Maybe.map
                    (fst
                        >> tabDisplay index mdl
                    )
                |> Maybe.withDefault defaultTab
            ]


quantityControlTable :
    String
    -> (a -> List (Html Msg))
    -> QuantityControl a Msg
    -> (a -> Html Msg)
    -> List Int
    -> Material.Model
    -> List a
    -> Html Msg
quantityControlTable typeHeading typeDisplay quantityControl elementView index mdl dataList =
    Table.table [ css "background-color" DCC.background ]
        [ Table.thead []
            [ Table.tr []
                [ Table.th [{- Table.onClick Reorder -}]
                    [ text "Deck Element" ]
                , Table.th [] [ text typeHeading ]
                , Table.th [ Table.numeric ] [ text "Quantity" ]
                ]
            ]
        , Table.tbody []
            (dataList
                |> Extras.uniqueMap
                    (\item ->
                        Table.tr []
                            [ Table.td []
                                [ elementView item
                                ]
                            , Table.td []
                                <| typeDisplay item
                            , Table.td []
                                [ quantityControl dataList item
                                ]
                            ]
                    )
            )
        ]


wrapWithTotal : Int -> List (Html msg) -> Html msg
wrapWithTotal total stuff =
    div
        [ style
            [ ( "display", "flex" )
            , ( "flex-direction", "column" )
            , ( "align-items", "center" )
            ]
        ]
        [ div []
            (div [ style [ ( "width", "100%" ), ( "text-align", "center" ), ( "background-color", DCC.background ) ] ]
                [ total
                    |> toString
                    |> ((++) "Total: ")
                    |> text
                ]
                :: stuff
            )
        ]


defaultTab =
    div [] [ text "Tab not found" ]


pieceDeckContolTabLabels =
    [ Tabs.textLabel []
        "Player"
    , Tabs.textLabel []
        "Computer"
    , Tabs.textLabel []
        "Both"
    , Tabs.textLabel []
        "None"
    ]
