module ExportModel exposing (..)

import GameEndCons exposing (GameEndCons(..), GamePredicate(..))
import PieceAppearances exposing (PieceAppearances, Appearance)
import Spaces exposing (Spaces, Space, SpaceType(..))
import Pieces exposing (Pieces, Piece, PieceType, Controller(..), MoveType(..), ProtoPiece(..))
import Json.Encode as Encode


type alias ExportModel =
    { gridWidth : Int
    , gridHeight : Int
    , spaceDeck : List SpaceType
    , pieceDeck : List ProtoPiece
    , moveTypeDeck : List MoveType
    , pieceAppearances : PieceAppearances
    , gameEndCons : GameEndCons
    , viewScale : Float
    }


defaultExportModel =
    { gridWidth = defaultWidth
    , gridHeight = defaultHeight
    , spaceDeck = defaultSpaceDeck
    , pieceDeck = defaultPieceTypeDeck
    , moveTypeDeck = defaultMoveTypeDeck
    , pieceAppearances = defaultpieceAppearances
    , gameEndCons =
        GameEndCons (NoPiecesControlledBy Computer)
            (NoPiecesControlledBy Player)
    , viewScale = 1.0
    }


toString : ExportModel -> String
toString exportModel =
    exportModel
        |> encode
        |> Encode.encode 4


encode : ExportModel -> Encode.Value
encode exportModel =
    Encode.object
        [ ( "gridWidth", Encode.int exportModel.gridWidth )
        , ( "gridHeight", Encode.int exportModel.gridHeight )
        , ( "spaceDeck", encodeMap (always <| Encode.string "TODO") exportModel.spaceDeck )
        , ( "pieceDeck", encodeMap (always <| Encode.string "TODO") exportModel.pieceDeck )
        , ( "moveTypeDeck", encodeMap (always <| Encode.string "TODO") exportModel.moveTypeDeck )
        , ( "pieceAppearances", Encode.string "TODO" )
          --exportModel.pieceAppearances)
        , ( "gameEndCons", Encode.string "TODO" )
          --exportModel.gameEndCons)
        , ( "viewScale", Encode.float exportModel.viewScale )
        ]


encodeMap : (a -> Encode.Value) -> List a -> Encode.Value
encodeMap f list =
    list
        |> List.map f
        |> Encode.list



-- exportModel
--     |> Basics.toString
--     |> Encode.string


defaultWidth =
    5


defaultHeight =
    5


defaultpieceAppearances : PieceAppearances
defaultpieceAppearances =
    Pieces.actualPieceTypePossibilities
        |> List.map PieceAppearances.pairWithAppearance
        |> PieceAppearances.fromList


defaultPieceTypeDeck =
    Pieces.actualPieceTypePossibilities
        |> List.filter (\p -> p.moveType == Pieces.AnySpace)
        |> List.map Pieces.ActualPiece
        |> (++)
            [ NoPiece
            , NoPiece
            ]



-- Pieces.protoPiecePossibilities
--     ++ [ NoPiece
--         , NoPiece
--        ]


defaultMoveTypeDeck =
    Pieces.moveTypePossibilities


defaultSpaceDeck =
    [ Green
    , Green
    , Red
    , Red
    , EmptySpace
    ]
