module AppearanceTab exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onInput)
import Model exposing (Model)
import Msg exposing (Msg(..))
import Material
import Material.Button as Button
import Material.Icon as Icon
import Material.Options as Options exposing (css)
import Material.Table as Table
import Material.Toggles as Toggles
import Material.Grid exposing (grid, cell, size, offset, Device(All, Tablet))
import Math.Vector2 as V2 exposing (Vec2, vec2)
import Pieces exposing (Piece, PieceType, ProtoPiece(..), Controller(..), MoveType(..), Shape(..))
import Spaces
import PieceAppearances exposing (PieceAppearances, Icon(..))
import Extras
import String
import DevControlsCommon as DCC
import Regex


render model =
    div []
        [ grid []
            [ cell [ size All 6 ]
                [ pieceAppearancesTable [ 14 ] model.mdl model.pieceAppearances
                ]
            ]
        ]


pieceAppearancesTable :
    List Int
    -> Material.Model
    -> PieceAppearances
    -> Html Msg
pieceAppearancesTable index mdl pieceAppearances =
    Table.table [ css "background-color" DCC.background ]
        [ Table.thead []
            [ Table.tr []
                [ Table.th [] [ text "preview" ]
                , Table.th [] [ text "piece type" ]
                , Table.th [] [ text "points" ]
                , Table.th [] [ text "colour" ]
                , Table.th [] [ text "icon" ]
                ]
            ]
        , Table.tbody []
            (pieceAppearances
                |> PieceAppearances.toList
                |> List.indexedMap
                    (\index ( pieceType, ( shape, colourString, icon ) ) ->
                        let
                            indexList =
                                [ index ]
                        in
                            Table.tr []
                                [ Table.td []
                                    [ pieceType
                                        |> DCC.positionedSvgMakerToHtmlMaker (DCC.pieceTypeToSVG pieceAppearances)
                                    ]
                                , Table.td []
                                    <| DCC.displayPiecetype pieceType
                                , Table.td []
                                    <| (case shape of
                                            Eye ->
                                                [ text "hardcoded" ]

                                            PointsList list ->
                                                editAblePointsList indexList
                                                    mdl
                                                    (EditPoints pieceType)
                                                    list
                                       )
                                , Table.td []
                                    [ Html.input
                                        [ cleanColourString
                                            >> UpdateColour pieceType
                                            |> onInput
                                        , colourString |> Html.Attributes.value
                                        , style [ ( "width", "6rem" ), ( "background-color", DCC.background ) ]
                                        ]
                                        []
                                    ]
                                , Table.td []
                                    [ Html.p []
                                        [ Toggles.radio Mdl
                                            ([ -10 ] ++ indexList)
                                            mdl
                                            [ Toggles.value (icon == EmptySpaceIcon)
                                            , Toggles.group "pieceAppearancesGroup"
                                            , Toggles.onClick (SetIcon EmptySpaceIcon pieceType)
                                            ]
                                            [ text "Empty space" ]
                                        ]
                                    , Html.p []
                                        [ Toggles.radio Mdl
                                            ([ -11 ] ++ indexList)
                                            mdl
                                            [ Toggles.value (icon == PieceAppearances.triangleIcon)
                                            , Toggles.group "pieceAppearancesGroup"
                                            , Toggles.onClick (SetIcon PieceAppearances.triangleIcon pieceType)
                                            ]
                                            [ text "Triangle on space" ]
                                        ]
                                    , Html.p []
                                        [ Toggles.radio Mdl
                                            ([ -12 ] ++ indexList)
                                            mdl
                                            [ Toggles.value (icon == NoIcon)
                                            , Toggles.group "pieceAppearancesGroup"
                                            , Toggles.onClick (SetIcon NoIcon pieceType)
                                            ]
                                            [ text "No icon" ]
                                        ]
                                    ]
                                ]
                    )
            )
        ]


editAblePointsList : List Int -> Material.Model -> (List Vec2 -> Msg) -> List Vec2 -> List (Html Msg)
editAblePointsList index mdl msgMaker list =
    [ Button.render Msg.Mdl
        (index ++ [ 0 ])
        mdl
        [ Button.onClick (msgMaker <| Maybe.withDefault [] <| List.tail list)
        ]
        [ Icon.i "remove"
        ]
    , Button.render Msg.Mdl
        (index ++ [ 1 ])
        mdl
        [ Button.onClick (msgMaker <| vec2 0 0 :: list)
        ]
        [ Icon.i "add"
        ]
    ]
        ++ List.indexedMap (editAblePoints msgMaker list) list


type V2Component
    = X
    | Y


pointsStyle =
    style [ ( "width", "4rem" ), ( "background-color", DCC.background ) ]


editAblePoints : (List Vec2 -> Msg) -> List Vec2 -> Int -> Vec2 -> Html Msg
editAblePoints msgMaker list index vector =
    Html.p []
        [ Html.input
            [ Html.Attributes.type' "number"
            , Html.Attributes.step "any"
            , updatePoint msgMaker list index vector X |> onInput
            , vector |> V2.getX |> toString |> Html.Attributes.value
            , pointsStyle
            ]
            []
        , text ","
        , Html.input
            [ Html.Attributes.type' "number"
            , Html.Attributes.step "any"
            , updatePoint msgMaker list index vector Y |> onInput
            , vector |> V2.getY |> toString |> Html.Attributes.value
            , pointsStyle
            ]
            []
        ]


updatePoint : (List Vec2 -> Msg) -> List Vec2 -> Int -> Vec2 -> V2Component -> String -> Msg
updatePoint msgMaker list index vector component inputString =
    case String.toFloat inputString of
        Ok newValue ->
            let
                newVector =
                    case component of
                        X ->
                            vec2 newValue (V2.getY vector)

                        Y ->
                            vec2 (V2.getX vector) newValue
            in
                Extras.setAt index newVector list
                    |> msgMaker

        Err _ ->
            msgMaker list


cleanColourString : String -> String
cleanColourString input =
    Regex.replace Regex.All (Regex.regex "[^#0-9A-Za-z]") (always "") input
