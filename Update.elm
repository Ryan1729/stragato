module Update exposing (update)

import Model exposing (Model, GamePredicate(..), GameEndCons(..), GameResult)
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
            let
                newPieces =
                    getNewPieces model pieceID spaceID
            in
                { model
                    | pieceSelected = Nothing
                    , pieces = newPieces
                    , gameResult = getGameResult model newPieces
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
                { model
                    | seed = newSeed
                    , spaces = spaces
                    , pieces = pieces
                    , gameResult = getGameResult model pieces
                }
                    ! []

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

        ToggleAllowMovingAllPieces ->
            { model | allowMovingAllPieces = not model.allowMovingAllPieces } ! []

        ToggleIgnoreGameResult ->
            { model | ignoreGameResult = not model.ignoreGameResult } ! []

        IncrementViewScale ->
            { model | viewScale = higherScale model.viewScale } ! []

        DecrementViewScale ->
            { model | viewScale = lowerScale model.viewScale } ! []

        DecrementWinCon ->
            { model | gameEndCons = Model.decrementWinCon model.gameEndCons } ! []

        IncrementWinCon ->
            { model | gameEndCons = Model.incrementWinCon model.gameEndCons } ! []

        DecrementLossCon ->
            { model | gameEndCons = Model.decrementLossCon model.gameEndCons } ! []

        IncrementLossCon ->
            { model | gameEndCons = Model.incrementLossCon model.gameEndCons } ! []

        MakeAIMove ->
            if Model.canMove model then
                randomAIMove model ! [ Ports.sound "clack" ]
            else
                model ! []

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


getNewPieces : Model -> Int -> SpaceIndex -> Pieces
getNewPieces model pieceID spaceID =
    if
        PiecesAndSpaces.canPieceMoveToSpace model.allowSelfMoves
            model.pieces
            model.spaces
            pieceID
            spaceID
    then
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
        ( Just piece, Just targetSpacePosition ) ->
            case
                ( piece.pieceType
                , Pieces.getPiecesAtPosition pieces targetSpacePosition
                )
            of
                {- They have a fight, Triangle wins. Triangle man! -}
                ( Triangle _, piecesOnSpace ) ->
                    pieces
                        |> Extras.filterOutListFromDict piecesOnSpace
                        |> Pieces.setPieceLocation index targetSpacePosition

                ( WeirdThing _, piecesOnSpace ) ->
                    pieces
                        |> bumpPieces spaces piece.position targetSpacePosition
                        |> Pieces.setPieceLocation index targetSpacePosition

                ( Eye _, piecesOnSpace ) ->
                    pieces
                        |> Pieces.movePieces targetSpacePosition piece.position
                        |> Pieces.setPieceLocation index targetSpacePosition

                ( Petals control, piecesOnSpace ) ->
                    if Pieces.noPiecesAtPosition pieces targetSpacePosition then
                        pieces
                            |> Pieces.setPieceLocation index targetSpacePosition
                            |> Pieces.addPiece (Piece (Petals control) piece.position)
                    else
                        pieces

                _ ->
                    if Pieces.noPiecesAtPosition pieces targetSpacePosition then
                        Pieces.setPieceLocation index targetSpacePosition pieces
                    else
                        pieces

        _ ->
            pieces


getGameResult : Model -> Pieces -> GameResult
getGameResult model pieces =
    case model.gameEndCons of
        GameEndCons winCondition loseCondition ->
            if checkPredicate model winCondition pieces then
                Model.Win
            else if checkPredicate model loseCondition pieces then
                Model.Loss
            else
                Model.TBD


checkPredicate : Model -> GamePredicate -> Pieces -> Bool
checkPredicate model predicate pieces =
    case predicate of
        NoPiecesControlledBy controllability ->
            Pieces.controllabiltyCount controllability pieces <= 0

        NoPiecesStrictlyControlledBy controllability ->
            Pieces.strictControllabiltyCount controllability pieces <= 0

        NoPiecesOfGivenTypeCanMove piecetype ->
            let
                matchingPieceIndicies =
                    Dict.toList pieces
                        |> List.filterMap
                            (\( index, piece ) ->
                                if piece.pieceType == piecetype then
                                    Just index
                                else
                                    Nothing
                            )

                indexPairs =
                    List.concatMap
                        (\spaceIndex ->
                            List.map
                                (\pieceIndex ->
                                    ( pieceIndex, spaceIndex )
                                )
                                matchingPieceIndicies
                        )
                        <| Dict.keys model.spaces
            in
                not
                    <| List.any
                        (\( index, spaceIndex ) ->
                            PiecesAndSpaces.canPieceMoveToSpace model.allowSelfMoves
                                pieces
                                model.spaces
                                index
                                spaceIndex
                        )
                        indexPairs


randomAIMove : Model -> Model
randomAIMove model =
    let
        moveList =
            getPossibleMoveList model
    in
        case Deck.maybeFromDeck moveList model.seed of
            Just ( ( pieceID, spaceID ), _, newSeed ) ->
                let
                    pieces =
                        getNewPieces model pieceID spaceID
                in
                    { model
                        | pieces = pieces
                        , seed = newSeed
                        , gameResult = getGameResult model pieces
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

        nonSelfMoves =
            cpuMovablePieces
                `Extras.andThen` \x ->
                                    case Dict.get x model.pieces of
                                        Just piece ->
                                            let
                                                availableIndicies =
                                                    case piece.pieceType of
                                                        Star _ ->
                                                            unoccupiedSpaceIndicies

                                                        _ ->
                                                            Spaces.getNonMatchingSpaceIndicies (Spaces.getActualSpaces model.spaces)
                                                                piece.position
                                            in
                                                availableIndicies
                                                    `Extras.andThen` \y ->
                                                                        [ ( x, y ) ]

                                        Nothing ->
                                            []
    in
        if model.allowSelfMoves then
            nonSelfMoves
                ++ PiecesAndSpaces.getSelfMoves cpuMovablePieces
                    model.pieces
                    model.spaces
        else
            nonSelfMoves


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
