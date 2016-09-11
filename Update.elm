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
import Dict exposing (Dict)


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


getNewPieces : Model -> Int -> ( Int, Int ) -> Dict Int Piece
getNewPieces model pieceId spaceId =
    case
        ( Dict.get pieceId model.pieces
        , PlayfieldComponents.getPosition spaceId model.spaces
        )
    of
        ( Just piece, Just spacePosition ) ->
            case
                ( piece.pieceType
                , getPiecesOnSpace model.pieces spacePosition
                )
            of
                {- They have a fight, Triangle wins. Triangle man! -}
                ( Triangle _, piecesOnSpace ) ->
                    model.pieces
                        |> removePiecesinList piecesOnSpace
                        |> setPieceLocation pieceId spacePosition

                ( WeirdThing _, piecesOnSpace ) ->
                    model.pieces
                        |> bumpPieces model.spaces piece.position spacePosition
                        |> setPieceLocation pieceId spacePosition

                _ ->
                    if spaceIsEmpty model spacePosition then
                        let
                            newPieces =
                                setPieceLocation pieceId spacePosition model.pieces
                        in
                            Dict.filter (Extras.ignoreFirstArg PlayfieldComponents.isActualPiece)
                                newPieces
                    else
                        model.pieces

        _ ->
            model.pieces


removePiecesinList : List Piece -> Dict Int Piece -> Dict Int Piece
removePiecesinList piecesToRemove pieces =
    Dict.filter
        (\index piece ->
            piece
                |> (flip List.member) piecesToRemove
                |> not
        )
        pieces


bumpPieces : Spaces -> Vec2 -> Vec2 -> Dict Int Piece -> Dict Int Piece
bumpPieces spaces piecePosition spacePosition pieces =
    case getTargetSpacePosition spaces piecePosition spacePosition of
        Just targetSpacePosition ->
            movePieces spacePosition targetSpacePosition pieces

        Nothing ->
            pieces


getTargetSpacePosition : Spaces -> Vec2 -> Vec2 -> Maybe Vec2
getTargetSpacePosition spaces piecePosition spacePosition =
    let
        maybeBumpingSpaceID =
            getSpaceFromPosition spaces piecePosition

        maybeBumpedSpaceID =
            getSpaceFromPosition spaces spacePosition
    in
        Maybe.map2 getTargetID maybeBumpingSpaceID maybeBumpedSpaceID
            `Maybe.andThen` (\targetID -> Dict.get targetID spaces)
            |> Maybe.map .position


getTargetID ( bumpingX, bumpingY ) ( bumpedX, bumpedY ) =
    ( bumpedX + (bumpedX - bumpingX), bumpedY + (bumpedY - bumpingY) )


getSpaceFromPosition : Spaces -> Vec2 -> Maybe ( Int, Int )
getSpaceFromPosition spaces targetPosition =
    spaces
        |> Dict.toList
        |> List.filterMap
            (\( index, space ) ->
                --TODO epsilon here?
                if space.position == targetPosition then
                    Just index
                else
                    Nothing
            )
        |> List.head


movePieces sourcePos targetPos pieces =
    Dict.map
        (\index piece ->
            --TODO should this be within epsilon?
            if piece.position == sourcePos then
                { piece | position = targetPos }
            else
                piece
        )
        pieces


spaceIsEmpty model spacePosition =
    let
        piecesOnSpace =
            getPiecesOnSpace model.pieces spacePosition
    in
        piecesOnSpace
            |> List.filter (PlayfieldComponents.isActualPiece)
            |> (==) []


getPiecesOnSpace : Dict Int Piece -> Vec2 -> List Piece
getPiecesOnSpace pieces spacePosition =
    pieces
        |> Dict.values
        |> List.filter (.position >> (==) spacePosition)


higherScale : Float -> Float
higherScale oldScale =
    oldScale + 0.5


lowerScale : Float -> Float
lowerScale oldScale =
    if oldScale > 1 then
        oldScale - 0.5
    else
        oldScale


setPieceLocation : Int -> Vec2 -> Dict Int Piece -> Dict Int Piece
setPieceLocation pieceId position pieces =
    Dict.update pieceId
        (Maybe.map
            (\piece ->
                { piece | position = position }
            )
        )
        pieces


v2FromPosition : Mouse.Position -> Vec2
v2FromPosition position =
    vec2 (toFloat position.x) (toFloat position.y)
