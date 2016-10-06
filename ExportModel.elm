module ExportModel exposing (..)

import GameEndCons exposing (GameEndCons(..), GamePredicate(..))
import PieceAppearances exposing (PieceAppearances, Appearance)
import Spaces exposing (Spaces, Space, SpaceType(..))
import Pieces exposing (Pieces, Piece, PieceType, Controller(..), MoveType(..), ProtoPiece(..), MoveEffect(..))
import Json.Encode as Encode
import PosInt


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
        , ( "spaceDeck", encodeMap encodeSpaceType exportModel.spaceDeck )
        , ( "pieceDeck", encodeMap encodeProtoPiece exportModel.pieceDeck )
        , ( "moveTypeDeck", encodeMap encodeMoveType exportModel.moveTypeDeck )
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


stringIt =
    Basics.toString >> Encode.string


encodeSpaceType : SpaceType -> Encode.Value
encodeSpaceType spaceType =
    spaceType |> stringIt


encodeMoveType : MoveType -> Encode.Value
encodeMoveType moveType =
    moveType |> stringIt


encodeMoveEffect : MoveEffect -> Encode.Value
encodeMoveEffect moveEffect =
    case moveEffect of
        Bump posInt ->
            "Bump "
                ++ (posInt |> PosInt.toInt |> Basics.toString)
                |> Encode.string

        simple ->
            simple
                |> stringIt


encodeProtoPiece : ProtoPiece -> Encode.Value
encodeProtoPiece protoPiece =
    case protoPiece of
        ActualPiece pieceType ->
            Encode.object
                [ ( "moveEffect", pieceType.moveEffect |> encodeMoveEffect )
                , ( "controller", pieceType.controller |> stringIt )
                , ( "moveType", pieceType.moveType |> encodeMoveType )
                ]

        NoPiece ->
            Encode.string "NoPiece"


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
