module Update exposing (..)

import Model exposing (Model)
import Msg exposing (Msg(..))
import Ports
import Mouse
import Math.Vector2 as V2 exposing (Vec2, vec2)
import Random
import Extras
import Material
import Spaces exposing (Spaces, SpaceType(..), SpaceIndex)
import Pieces exposing (Pieces, Piece, PieceType(..))
import PiecesAndSpaces
import Dict exposing (Dict)
import Deck


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        HitTable ->
            { model | pieceSelected = Nothing } ! [ Ports.sound "tableHit" ]

        SelectPiece id ->
            { model | pieceSelected = Just id } ! []

        ClearPieceSelection ->
            { model | pieceSelected = Nothing } ! []

        MovePiece pieceID spaceID ->
            { model
                | pieceSelected = Nothing
                , pieces = getNewPieces model pieceID spaceID
            }
                ! [ Ports.sound "clack" ]

        GenerateBoard ->
            let
                ( spaces, postSpacesSeed ) =
                    Model.makeSpaces model.gridWidth
                        model.gridHeight
                        model.spaceDeck
                        model.seed

                ( pieces, newSeed ) =
                    Model.makePieces spaces
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

        ToggleSelfMoves ->
            { model | allowSelfMoves = not model.allowSelfMoves } ! []

        IncrementViewScale ->
            { model | viewScale = higherScale model.viewScale } ! []

        DecrementViewScale ->
            { model | viewScale = lowerScale model.viewScale } ! []

        MakeAIMove ->
            randomAIMove model ! [ Ports.sound "clack" ]

        --TODO animate something or remove this!
        Animate _ ->
            model ! []

        Resize size ->
            { model | windowSize = size } ! []

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
            Material.update msg'
                model



--TODO make this return Cmd


getNewPieces : Model -> Int -> SpaceIndex -> Pieces
getNewPieces model pieceID spaceID =
    if model.allowSelfMoves || PiecesAndSpaces.pieceIsNotAtSpace model.pieces model.spaces pieceID spaceID then
        movePieceToSpace model.pieces model.spaces pieceID spaceID
    else
        model.pieces


movePieceToSpace : Pieces -> Spaces -> Int -> SpaceIndex -> Pieces
movePieceToSpace pieces spaces index spaceIndex =
    case
        ( Dict.get index pieces
        , Spaces.getPosition spaceIndex spaces
        )
    of
        ( Just piece, Just spacePosition ) ->
            case
                ( piece.pieceType
                , Pieces.getPiecesAtPosition pieces spacePosition
                )
            of
                {- They have a fight, Triangle wins. Triangle man! -}
                ( Triangle _, piecesOnSpace ) ->
                    pieces
                        |> Extras.filterOutListFromDict piecesOnSpace
                        |> Pieces.setPieceLocation index spacePosition

                ( WeirdThing _, piecesOnSpace ) ->
                    pieces
                        |> bumpPieces spaces piece.position spacePosition
                        |> Pieces.setPieceLocation index spacePosition

                ( Eye _, piecesOnSpace ) ->
                    pieces
                        |> Pieces.movePieces spacePosition piece.position
                        |> Pieces.setPieceLocation index spacePosition

                _ ->
                    if Pieces.noPiecesAtPosition pieces spacePosition then
                        Pieces.setPieceLocation index spacePosition pieces
                    else
                        pieces

        _ ->
            pieces


randomAIMove : Model -> Model
randomAIMove model =
    let
        moveList =
            getPossibleMoveList model
    in
        case Deck.maybeFromDeck moveList model.seed of
            Just ( ( pieceID, spaceID ), _, newSeed ) ->
                { model
                    | pieces = getNewPieces model pieceID spaceID
                    , seed = newSeed
                }

            Nothing ->
                model


getPossibleMoveList : Model -> List ( Int, SpaceIndex )
getPossibleMoveList model =
    let
        unoccupiedSpaceIndicies =
            PiecesAndSpaces.getUnoccupiedSpaceIndicies model.pieces model.spaces

        cpuMovablePieces =
            Pieces.getCPUMovablePieces model.pieces

        movesToUnoccupiedSpaces =
            cpuMovablePieces
                `Extras.andThen` \x ->
                                    unoccupiedSpaceIndicies
                                        `Extras.andThen` \y ->
                                                            [ ( x, y ) ]
    in
        if model.allowSelfMoves then
            movesToUnoccupiedSpaces
                ++ PiecesAndSpaces.getSelfMoves cpuMovablePieces
                    model.pieces
                    model.spaces
        else
            movesToUnoccupiedSpaces


bumpPieces : Spaces -> Vec2 -> Vec2 -> Pieces -> Pieces
bumpPieces spaces piecePosition spacePosition pieces =
    case getTargetSpacePosition spaces piecePosition spacePosition of
        Just targetSpacePosition ->
            if Spaces.positionIsOnActualSpace spaces targetSpacePosition then
                Pieces.movePieces spacePosition targetSpacePosition pieces
            else
                Pieces.removePiecesAtPosition spacePosition pieces

        Nothing ->
            Pieces.removePiecesAtPosition spacePosition pieces


getTargetSpacePosition : Spaces -> Vec2 -> Vec2 -> Maybe Vec2
getTargetSpacePosition spaces piecePosition spacePosition =
    let
        maybeBumpingSpaceID =
            Spaces.getSpaceFromPosition spaces piecePosition

        maybeBumpedSpaceID =
            Spaces.getSpaceFromPosition spaces spacePosition
    in
        Maybe.map2 getTargetID maybeBumpingSpaceID maybeBumpedSpaceID
            `Maybe.andThen` (\targetID -> Dict.get targetID spaces)
            |> Maybe.map .position


getTargetID ( bumpingX, bumpingY ) ( bumpedX, bumpedY ) =
    ( bumpedX + (bumpedX - bumpingX), bumpedY + (bumpedY - bumpingY) )


higherScale : Float -> Float
higherScale oldScale =
    oldScale + 0.5


lowerScale : Float -> Float
lowerScale oldScale =
    if oldScale > 1 then
        oldScale - 0.5
    else
        oldScale
