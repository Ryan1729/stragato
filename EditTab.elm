module EditTab exposing (..)

import Html exposing (Html, div, text)
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
import DevControlsCommon as DCC


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
                    (toString model.exportModel.gridWidth)
            , cell [ size All 4 ]
                <| makeStepper [ 6 ]
                    model.mdl
                    "gridHeight"
                    DecrementGridHeight
                    IncrementGridHeight
                    (toString model.exportModel.gridHeight)
            , cell [ size All 4 ]
                <| makeStepper [ 7 ]
                    model.mdl
                    "scale"
                    DecrementViewScale
                    IncrementViewScale
                    (toString model.exportModel.viewScale)
            ]
        , grid []
            [ cell [ size All 6 ]
                [ exportDeckControl [ 2 ]
                    model.mdl
                    Spaces.spaceTypePossibilities
                    model.exportModel.spaceDeck
                    "space type"
                    DCC.displaySpaceType
                    Msg.SpaceDeckDecrement
                    Msg.SpaceDeckIncrement
                    (DCC.positionedSvgMakerToHtmlMaker
                        <| Playfield.space model.showSpaceOutlines [ stroke "grey" ]
                    )
                ]
            , cell [ size All 6 ]
                [ exportDeckControl [ 3 ]
                    model.mdl
                    Pieces.protoPiecePossibilities
                    model.exportModel.pieceDeck
                    "piece type"
                    displayProtoPieceType
                    Msg.PieceDeckDecrement
                    Msg.PieceDeckIncrement
                    (DCC.positionedSvgMakerToHtmlMaker
                        <| protoPieceToSVG model.exportModel.pieceAppearances
                    )
                ]
            ]
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


makeStepper : List Int -> Material.Model -> String -> ExportMsg -> ExportMsg -> String -> List (Html Msg)
makeStepper index mdl label decrementMsg incrementMsg value =
    [ text label
    , div [ style [ ( "border", "1px solid" ) ] ]
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
            , Html.li [ style [ ( "margin-left", "2em" ) ] ] [ text value ]
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
    -> (a -> Int -> Msg)
    -> (a -> Int -> Msg)
    -> (a -> Html Msg)
    -> Html Msg
deckControl index mdl possibilities currentDeck typeHeading typeDisplay removeMessage addMessage elementView =
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
                                [ let
                                    currentAmount =
                                        amountOfItemInDeck item currentDeck
                                  in
                                    Html.input
                                        [ Html.Attributes.type' "number"
                                        , Html.Attributes.min "0"
                                        , Html.Attributes.step "any"
                                        , String.toInt
                                            >> Result.withDefault currentAmount
                                            >> Debug.log ""
                                            >> (\newAmount ->
                                                    if newAmount > currentAmount then
                                                        addMessage item <| Debug.log "diff add" (newAmount - currentAmount)
                                                    else if newAmount < currentAmount then
                                                        removeMessage item <| Debug.log "diff remove" (currentAmount - newAmount)
                                                    else
                                                        NoOp
                                               )
                                            |> onInput
                                        , currentAmount |> toString |> Html.Attributes.value
                                        , style [ ( "width", "4rem" ), ( "background-color", DCC.background ) ]
                                        ]
                                        []
                                ]
                            ]
                    )
            )
        ]


exportDeckControl :
    List Int
    -> Material.Model
    -> List a
    -> List a
    -> String
    -> (a -> List (Html Msg))
    -> (a -> Int -> ExportMsg)
    -> (a -> Int -> ExportMsg)
    -> (a -> Html Msg)
    -> Html Msg
exportDeckControl index mdl possibilities currentDeck typeHeading typeDisplay removeMessage addMessage elementView =
    deckControl index
        mdl
        possibilities
        currentDeck
        typeHeading
        typeDisplay
        (UpdateExportModel ... removeMessage)
        (UpdateExportModel ... addMessage)
        elementView



-- the blackbird combinator
-- see https://youtu.be/seVSlKazsNk?t=11m53s


(...) : (a -> b) -> (c -> d -> a) -> c -> d -> b
(...) =
    (<<) << (<<)


amountOfItemInDeck : a -> List a -> Int
amountOfItemInDeck item deck =
    List.filter ((==) item) deck
        |> List.length
