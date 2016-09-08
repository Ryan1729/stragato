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
                , pieces = getNewPieces model pieceId spaceId
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

        ToggleSpaceOutlines ->
            { model | showSpaceOutlines = not model.showSpaceOutlines } ! []

        IncrementViewScale ->
            { model | viewScale = higherScale model.viewScale } ! []

        DecrementViewScale ->
            { model | viewScale = lowerScale model.viewScale } ! []

        Animate _ ->
            model ! []

        GetSeed time ->
            { model
                | seed =
                    Random.initialSeed
                        <| Debug.log "seed"
                        <| round time
            }
                ! []

        -- When the `Mdl` messages come through, update appropriately.
        Mdl msg' ->
            -- Material.update msg'
            model ! []


getNewPieces : Model -> Int -> Int -> Array Piece
getNewPieces model pieceId spaceId =
    case Array.get spaceId model.spaces.positions of
        Just spacePosition ->
            if canMoveTo model pieceId spacePosition then
                setPieceLocation model.pieces pieceId spacePosition
            else
                model.pieces

        Nothing ->
            model.pieces


canMoveTo model pieceId spacePosition =
    let
        piecesOnSpace =
            Debug.log "" <| getPiecesOnSpace model spacePosition
    in
        Array.length piecesOnSpace <= 0


getPiecesOnSpace model spacePosition =
    model.pieces
        |> Array.filter (PlayfieldComponents.isActualPiece)
        |> Array.map .position
        |> Array.filter ((==) spacePosition)


higherScale : Float -> Float
higherScale oldScale =
    oldScale + 0.5


lowerScale : Float -> Float
lowerScale oldScale =
    if oldScale > 1 then
        oldScale - 0.5
    else
        oldScale


setPieceLocation : Array Piece -> Int -> Vec2 -> Array Piece
setPieceLocation pieces pieceId position =
    Array.Extra.update pieceId
        (\piece ->
            { piece | position = position }
        )
        pieces


v2FromPosition : Mouse.Position -> Vec2
v2FromPosition position =
    vec2 (toFloat position.x) (toFloat position.y)
