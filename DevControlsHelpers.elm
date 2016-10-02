module DevControlsHelpers exposing (..)

import Html exposing (Html)
import Msg exposing (Msg)
import Material.Grid exposing (grid, cell, size, offset, Device(All, Tablet))
import Math.Vector2 as V2 exposing (Vec2, vec2)
import Svg exposing (Svg, svg)
import Svg.Attributes exposing (height, width, viewBox, stroke)
import Playfield
import Pieces exposing (Piece, PieceType, ProtoPiece(..), Controller(..), MoveType(..), Shape(..))
import PieceAppearances exposing (PieceAppearances)


positionedSvgMakerToHtmlMaker : (Vec2 -> a -> Svg Msg) -> a -> Html Msg
positionedSvgMakerToHtmlMaker svgMaker identifier =
    svg [ width "50", height "50", viewBox "0 0 150 150" ]
        [ svgMaker (vec2 75 75) identifier ]


pieceTypeToSVG : PieceAppearances -> Vec2 -> PieceType -> Svg Msg
pieceTypeToSVG pieceAppearances center pieceType =
    Playfield.piece pieceAppearances [ stroke "grey" ] center pieceType
