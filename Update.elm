module Update exposing (..)

import Model exposing (Model)
import Msg exposing (Msg(..))
import Ports
import Mouse
import Math.Vector2 as V2 exposing (Vec2, vec2)
import Array exposing (Array)
import Array.Extra
import Random
import Extras
import Material
import PlayfieldComponents exposing (Piece, PieceType(..), Spaces, SpaceType(..))


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        HitTable ->
            model ! [ Ports.sound "tableHit" ]

        SelectPiece id ->
            { model | pieceSelected = Just id } ! []

        ClearPieceSelection ->
            { model | pieceSelected = Nothing } ! []

        MovePiece pieceId spaceId ->
            { model
                | pieceSelected = Nothing
                , pieces =
                    setPieceLocation model.pieces pieceId
                        <| Array.get spaceId model.spaces.positions
            }
                ! [ Ports.sound "clack" ]

        GenerateBoard ->
            let
                ( spaces, postSpacesSeed ) =
                    PlayfieldComponents.makeSpaces model.gridWidth
                        model.gridHeight
                        model.spaceDeck
                        model.seed

                ( pieces, newSeed ) =
                    PlayfieldComponents.makePieces spaces
                        model.pieceDeck
                        postSpacesSeed
            in
                { model | seed = newSeed, spaces = spaces, pieces = pieces } ! []

        SelectTab tabIndex ->
            { model | tabIndex = tabIndex } ! []

        SpaceDeckIncrement item ->
            { model | spaceDeck = item :: model.spaceDeck } ! []

        SpaceDeckDecrement item ->
            { model | spaceDeck = Extras.remove item model.spaceDeck } ! []

        PieceDeckIncrement item ->
            { model | pieceDeck = item :: model.pieceDeck } ! []

        PieceDeckDecrement item ->
            { model | pieceDeck = Extras.remove item model.pieceDeck } ! []

        IncrementGridWidth ->
            { model | gridWidth = model.gridWidth + 1 } ! []

        DecrementGridWidth ->
            { model | gridWidth = max 0 (model.gridWidth - 1) } ! []

        IncrementGridHeight ->
            { model | gridHeight = model.gridHeight + 1 } ! []

        DecrementGridHeight ->
            { model | gridHeight = max 0 (model.gridHeight - 1) } ! []

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
            Array.Extra.update pieceId
                (\piece ->
                    { piece | position = position }
                )
                pieces

        Nothing ->
            pieces


v2FromPosition : Mouse.Position -> Vec2
v2FromPosition position =
    vec2 (toFloat position.x) (toFloat position.y)
