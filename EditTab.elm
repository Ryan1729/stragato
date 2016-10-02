module EditTab exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Model exposing (Model)
import Msg exposing (Msg(..))
import Material
import Material.Options as Options exposing (css)
import Material.Button as Button
import Material.Icon as Icon
import Material.List as Lists
import Material.Table as Table
import Material.Toggles as Toggles
import Material.Grid exposing (grid, cell, size, offset, Device(All, Tablet))
import Math.Vector2 as V2 exposing (Vec2, vec2)
import Svg exposing (Svg, svg)
import Svg.Attributes exposing (height, width, viewBox, stroke)
import Playfield
import Pieces exposing (Piece, PieceType, ProtoPiece(..), Controller(..), MoveType(..), Shape(..))
import Spaces
import PieceAppearances exposing (PieceAppearances)
import String
import DevControlsCommon


render : Model -> Html Msg
render model =
    div []
        [ grid []
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
            ]
        , grid []
            [ toggleSwitchCell [ 8 ]
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
            ]
        , grid []
            [ cell [ size All 4 ]
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
            ]
        , grid []
            [ cell [ size All 6 ]
                [ deckControl [ 2 ]
                    model.mdl
                    Spaces.spaceTypePossibilities
                    model.spaceDeck
                    "space type"
                    (always <| [ text "not implemented" ])
                    Msg.SpaceDeckDecrement
                    Msg.SpaceDeckIncrement
                    (DevControlsCommon.positionedSvgMakerToHtmlMaker
                        <| Playfield.space model.showSpaceOutlines [ stroke "grey" ]
                    )
                ]
            , cell [ size All 6 ]
                [ deckControl [ 3 ]
                    model.mdl
                    Pieces.protoPiecePossibilities
                    model.pieceDeck
                    "piece type"
                    displayProtoPieceType
                    Msg.PieceDeckDecrement
                    Msg.PieceDeckIncrement
                    (DevControlsCommon.positionedSvgMakerToHtmlMaker
                        <| protoPieceToSVG model.pieceAppearances
                    )
                ]
            ]
        ]


displayProtoPieceType : ProtoPiece -> List (Html Msg)
displayProtoPieceType protoPiece =
    case protoPiece of
        ActualPiece pieceType ->
            DevControlsCommon.displayPiecetype pieceType

        NoPiece ->
            [ text "No piece" ]


protoPieceToSVG : PieceAppearances -> Vec2 -> ProtoPiece -> Svg Msg
protoPieceToSVG pieceAppearances center protoPiece =
    case protoPiece of
        ActualPiece pieceType ->
            DevControlsCommon.pieceTypeToSVG pieceAppearances center pieceType

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


makeStepper : List Int -> Material.Model -> String -> Msg -> Msg -> String -> List (Html Msg)
makeStepper index mdl label decrementMsg incrementMsg value =
    [ text label
    , div [ style [ ( "border", "1px solid" ) ] ]
        [ Html.ul [ style [ ( "list-style", "none" ), ( "padding", "0" ), ( "margin", "0" ) ] ]
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
        ]
    ]


tup =
    (,)


deckControl :
    List Int
    -> Material.Model
    -> List a
    -> List a
    -> String
    -> (a -> List (Html Msg))
    -> (a -> Msg)
    -> (a -> Msg)
    -> (a -> Html Msg)
    -> Html Msg
deckControl index mdl possibilities currentDeck typeHeading typeDisplay addMessage removeMessage elementView =
    Table.table [ css "background-color" "#DDDDDD" ]
        [ Table.thead []
            [ Table.tr []
                [ Table.th [{- Table.onClick Reorder -}]
                    [ text "Deck Element" ]
                , Table.th [] [ text typeHeading ]
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
                                <| typeDisplay item
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
