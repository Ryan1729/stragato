module AppearanceTab exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onInput)
import Model exposing (Model)
import Msg exposing (Msg(..))
import Material
import Material.Options as Options exposing (css)
import Material.Icon as Icon
import Material.Table as Table
import Material.Grid exposing (grid, cell, size, offset, Device(All, Tablet))
import Math.Vector2 as V2 exposing (Vec2, vec2)
import Pieces exposing (Piece, PieceType, ProtoPiece(..), Controller(..), MoveType(..), Shape(..))
import Spaces
import PieceAppearances exposing (PieceAppearances)
import Extras
import String
import DevControlsHelpers


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
    Table.table [ css "background-color" "#DDDDDD" ]
        [ Table.thead []
            [ Table.tr []
                [ Table.th [] [ text "preview" ]
                , Table.th [] [ text "piece type" ]
                , Table.th [] [ text "points" ]
                , Table.th [] [ text "colour" ]
                ]
            ]
        , Table.tbody []
            (pieceAppearances
                |> PieceAppearances.toList
                |> List.map
                    (\( pieceType, ( shape, colourString ) ) ->
                        Table.tr []
                            [ Table.td []
                                [ DevControlsHelpers.positionedSvgMakerToHtmlMaker (DevControlsHelpers.pieceTypeToSVG pieceAppearances)
                                    pieceType
                                ]
                            , Table.td []
                                <| displayPiecetype pieceType
                            , Table.td []
                                <| (case shape of
                                        Eye ->
                                            [ text "hardcoded" ]

                                        PointsList list ->
                                            editAblePointsList (EditPoints pieceType) list
                                   )
                            , Table.td []
                                [ text colourString
                                ]
                            ]
                    )
            )
        ]


editAblePointsList : (List Vec2 -> Msg) -> List Vec2 -> List (Html Msg)
editAblePointsList msgMaker list =
    List.indexedMap (editAblePoints msgMaker list) list


type V2Component
    = X
    | Y


editAblePoints : (List Vec2 -> Msg) -> List Vec2 -> Int -> Vec2 -> Html Msg
editAblePoints msgMaker list index vector =
    Html.p []
        [ Html.input
            [ Html.Attributes.type' "number"
            , Html.Attributes.step "any"
            , updatePoint msgMaker list index vector X |> onInput
            , vector |> V2.getX |> toString |> Html.Attributes.value
            , style [ ( "width", "4rem" ) ]
            ]
            []
        , text ","
        , Html.input
            [ Html.Attributes.type' "number"
            , Html.Attributes.step "any"
            , updatePoint msgMaker list index vector Y |> onInput
            , vector |> V2.getY |> toString |> Html.Attributes.value
            , style [ ( "width", "4rem" ) ]
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


roundOff : Float -> Float
roundOff =
    Extras.roundTo 2


displayPiecetype : PieceType -> List (Html Msg)
displayPiecetype pieceType =
    [ pOf pieceType.moveEffect
    , pOf pieceType.controller
    , pOf pieceType.moveType
    ]


pOf : a -> Html Msg
pOf thing =
    Html.p [] [ toString thing |> text ]