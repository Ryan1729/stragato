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
import Movement


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
                    Movement.getNewPieces model pieceID spaceID
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


randomAIMove : Model -> Model
randomAIMove model =
    let
        moveList =
            Movement.getPossibleMoveList model
    in
        case Deck.maybeFromDeck moveList model.seed of
            Just ( ( pieceID, spaceID ), _, newSeed ) ->
                let
                    pieces =
                        Movement.getNewPieces model pieceID spaceID
                in
                    { model
                        | pieces = pieces
                        , seed = newSeed
                        , gameResult = getGameResult model pieces
                    }

            Nothing ->
                model


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


higherScale : Float -> Float
higherScale oldScale =
    oldScale + 0.5


lowerScale : Float -> Float
lowerScale oldScale =
    if oldScale > 1 then
        oldScale - 0.5
    else
        oldScale
