module Update exposing (..)

import Model exposing (Model, PieceDrag)
import Msg exposing (Msg(..))
import Ports
import Mouse
import Math.Vector2 as V2 exposing (Vec2, vec2)


update : Msg -> Model -> ( Model, Cmd msg )
update message model =
    case message of
        PlayClack ->
            model ! [ Ports.sound "clack" ]

        PieceDragStart position ->
            { model | pieceDrag = Just <| PieceDrag <| Debug.log "pos" position } ! []

        PieceDragMove position ->
            case model.pieceDrag of
                Just drag ->
                    { model
                        | pieceDrag = Just <| PieceDrag position
                    }
                        ! []

                Nothing ->
                    model ! []

        PieceDragEnd _ ->
            { model | pieceDrag = Nothing } ! []

        Animate _ ->
            case model.pieceDrag of
                Just drag ->
                    { model
                        | piecePosition = v2FromPosition drag.position
                    }
                        ! []

                Nothing ->
                    model ! []


v2FromPosition : Mouse.Position -> Vec2
v2FromPosition position =
    vec2 (toFloat position.x) (toFloat position.y)
