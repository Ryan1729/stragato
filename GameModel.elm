module GameModel exposing (..)

import Model
import ExportModel exposing (ExportModel)
import TransferModel exposing (TransferModel)
import Spaces exposing (Spaces, Space, SpaceType(..), SpaceIndex)
import Pieces exposing (Pieces)
import Random exposing (Seed)


type alias Model =
    { pieceSelected : Maybe Int
    , gameResult : GameResult
    , exportModel : ExportModel
    , pieces : Pieces
    , spaces : Spaces
    , seed : Seed
    , ignoreGameResult : Bool
    , showSpaceOutlines : Bool
    , allowMovingAllPieces : Bool
    }


defaultState =
    { pieceSelected = Nothing
    , gameResult = TBD
    , exportModel = ExportModel.defaultExportModel
    , pieces = Model.defaultPieces
    , spaces = Model.defaultSpaces
    , seed = (Random.initialSeed 42)
    , ignoreGameResult = False
    , showSpaceOutlines = False
    , allowMovingAllPieces = False
    }



--TODO: should draws be possible?


type GameResult
    = Win
    | Loss
    | TBD


canMove : Model -> Bool
canMove model =
    model.ignoreGameResult || model.gameResult == TBD


applyTransferModelToGameModel : Model -> TransferModel -> Model
applyTransferModelToGameModel gameModel transferModel =
    { gameModel
        | exportModel = transferModel.exportModel
        , pieces = Maybe.withDefault gameModel.pieces transferModel.pieces
        , spaces = Maybe.withDefault gameModel.spaces transferModel.spaces
        , ignoreGameResult = transferModel.ignoreGameResult
        , showSpaceOutlines = transferModel.showSpaceOutlines
        , allowMovingAllPieces = transferModel.allowMovingAllPieces
    }


applyExportModelToGameModel : Model -> ExportModel -> Model
applyExportModelToGameModel gameModel exportModel =
    { gameModel | exportModel = exportModel }
