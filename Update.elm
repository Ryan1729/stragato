module Update exposing (..)

import Model exposing (Model)
import Msg exposing (Msg(..))
import Ports
import Mouse
import Math.Vector2 as V2 exposing (Vec2, vec2)


update : Msg -> Model -> ( Model, Cmd msg )
update message model =
    case message of
        PlayClack ->
            model ! [ Ports.sound "clack" ]

        SelectPiece id ->
            { model | pieceSelected = Just id } ! []

        SpaceClicked id ->
            { model | pieceSelected = Nothing } ! []

        Animate _ ->
            model ! []


v2FromPosition : Mouse.Position -> Vec2
v2FromPosition position =
    vec2 (toFloat position.x) (toFloat position.y)
