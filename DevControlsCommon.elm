module DevControlsCommon exposing (..)

import Html exposing (Html, text)
import Msg exposing (Msg)
import Material.Grid exposing (grid, cell, size, offset, Device(All, Tablet))
import Math.Vector2 as V2 exposing (Vec2, vec2)
import Svg exposing (Svg, svg)
import Svg.Attributes exposing (height, width, viewBox, stroke)
import Playfield
import Pieces exposing (Piece, PieceType, ProtoPiece(..), Controller(..), MoveType(..), Shape(..))
import PieceAppearances exposing (PieceAppearances)
import Spaces exposing (SpaceType)


positionedSvgMakerToHtmlMaker : (Vec2 -> a -> Svg Msg) -> a -> Html Msg
positionedSvgMakerToHtmlMaker svgMaker identifier =
    svg [ width "50", height "50", viewBox "0 0 150 150" ]
        [ svgMaker (vec2 75 75) identifier ]


pieceTypeToSVG : PieceAppearances -> Vec2 -> PieceType -> Svg Msg
pieceTypeToSVG pieceAppearances center pieceType =
    Playfield.piece pieceAppearances [ stroke "grey" ] center pieceType


displayPiecetype : PieceType -> List (Html Msg)
displayPiecetype pieceType =
    pieceType
        |> Pieces.pieceTypeToStringList
        |> List.map pOfString


displaySpaceType : SpaceType -> List (Html Msg)
displaySpaceType spaceType =
    [ pOf spaceType
    ]


pOf : a -> Html Msg
pOf =
    toString >> pOfString


pOfString s =
    Html.p [] [ s |> text ]


background =
    "#DDDDDD"
