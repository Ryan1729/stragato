module Update exposing (..)

import Model exposing (Model, Piece)
import Msg exposing (Msg(..))
import Ports
import Mouse
import Math.Vector2 as V2 exposing (Vec2, vec2)
import Array
import Random


update : Msg -> Model -> ( Model, Cmd msg )
update message model =
    case message of
        PlayClack ->
            model ! [ Ports.sound "clack" ]

        SelectPiece id ->
            { model | pieceSelected = Just id } ! []

        MovePiece pieceId spaceId ->
            { model
                | pieceSelected = Nothing
                , pieceList =
                    setPieceLocation model.pieceList pieceId
                        <| Array.get spaceId model.spaces.positions
            }
                ! []

        Animate _ ->
            model ! []

        GetSeed time ->
            { model
                | seed = Random.initialSeed <| Debug.log "seed" <| round time
            }
                ! []


setPieceLocation : List Piece -> Int -> Maybe Vec2 -> List Piece
setPieceLocation pieces pieceId maybePosition =
    --TODO switch to arrays later
    case maybePosition of
        Just position ->
            List.indexedMap
                (\index piece ->
                    if index == pieceId then
                        { piece | position = position }
                    else
                        piece
                )
                pieces

        Nothing ->
            pieces


v2FromPosition : Mouse.Position -> Vec2
v2FromPosition position =
    vec2 (toFloat position.x) (toFloat position.y)
