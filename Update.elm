module Update exposing (..)

import Model exposing (Model, Piece)
import Msg exposing (Msg(..))
import Ports
import Mouse
import Math.Vector2 as V2 exposing (Vec2, vec2)
import Array exposing (Array)
import Random
import Extras
import Material


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        PlayClack ->
            model ! [ Ports.sound "clack" ]

        SelectPiece id ->
            { model | pieceSelected = Just id } ! []

        MovePiece pieceId spaceId ->
            { model
                | pieceSelected = Nothing
                , pieces =
                    setPieceLocation model.pieces pieceId
                        <| Array.get spaceId model.spaces.positions
            }
                ! []

        GenerateBoard ->
            let
                ( spaces, newSeed ) =
                    Model.makeSpaces model.gridWidth
                        model.gridHeight
                        model.spaceDeck
                        model.seed
            in
                { model | seed = newSeed, spaces = spaces } ! []

        SelectTab tabIndex ->
            { model | tabIndex = tabIndex } ! []

        Animate _ ->
            model ! []

        GetSeed time ->
            { model
                | seed = Random.initialSeed <| Debug.log "seed" <| round time
            }
                ! []

        -- When the `Mdl` messages come through, update appropriately.
        Mdl msg' ->
            -- Material.update msg'
            model ! []


setPieceLocation : Array Piece -> Int -> Maybe Vec2 -> Array Piece
setPieceLocation pieces pieceId maybePosition =
    case maybePosition of
        Just position ->
            Extras.update pieceId
                (\piece ->
                    { piece | position = position }
                )
                pieces

        Nothing ->
            pieces


v2FromPosition : Mouse.Position -> Vec2
v2FromPosition position =
    vec2 (toFloat position.x) (toFloat position.y)
