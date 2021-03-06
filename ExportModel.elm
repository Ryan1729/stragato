module ExportModel exposing (..)

import GameEndCons exposing (GameEndCons(..), GamePredicate(..))
import PieceAppearances exposing (PieceAppearances, Appearance, AppearancePair)
import Spaces exposing (Spaces, Space, SpaceType(..))
import Pieces exposing (Pieces, Piece, Shape(..), PieceType, Controller(..), MoveOccupancy(..), ProtoPiece(..), MoveEffect(..))
import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder, (:=))
import PosInt
import String
import CommonDecoders exposing (..)
import CommonEncoders exposing (..)
import Math.Vector2 as V2 exposing (Vec2, vec2)
import Version


type alias ExportModel =
    { gridWidth : Int
    , gridHeight : Int
    , spaceDeck : List SpaceType
    , pieceDeck : List ProtoPiece
    , gameEndCons : GameEndCons
    , viewScale : Float
    , pieceAppearances : PieceAppearances
    }


defaultExportModel =
    { gridWidth = defaultWidth
    , gridHeight = defaultHeight
    , spaceDeck = defaultSpaceDeck
    , pieceDeck = defaultPieceTypeDeck
    , gameEndCons = defaultGameEndCons
    , viewScale = defaultViewScale
    , pieceAppearances = defaultPieceAppearances
    }


defaultWidth =
    5


defaultHeight =
    5


defaultViewScale =
    1.0


defaultGameEndCons =
    GameEndCons defaultWinCon
        defaultLossCon


defaultWinCon =
    (NoPiecesControlledBy Computer)


defaultLossCon =
    (NoPiecesControlledBy Player)


defaultPieceAppearances : PieceAppearances
defaultPieceAppearances =
    Pieces.actualPieceTypePossibilities
        |> List.map PieceAppearances.pairWithAppearance
        |> PieceAppearances.fromList


defaultPieceTypeDeck =
    [ NoPiece
    , NoPiece
    , ActualPiece { moveEffect = Copy, controller = None, movePattern = { occupied = [ ( 0, 4 ), ( 1, 5 ), ( 2, 6 ), ( 2, -2 ), ( 2, 0 ), ( 2, 2 ), ( 2, 2 ), ( 3, 3 ), ( 4, 4 ), ( 4, 0 ), ( 5, -1 ), ( 6, -2 ), ( -2, 2 ), ( -2, 0 ), ( -2, -2 ), ( -2, -2 ), ( -1, -3 ), ( 0, -4 ) ], unoccupied = [ ( 0, 2 ), ( 1, -1 ), ( 1, 1 ), ( 2, 0 ), ( -1, 1 ), ( -1, -1 ) ] } }
    , ActualPiece { moveEffect = Copy, controller = None, movePattern = { occupied = [ ( 0, 4 ), ( 1, 5 ), ( 2, 6 ), ( 2, -2 ), ( 2, 0 ), ( 2, 2 ), ( 2, 2 ), ( 3, 3 ), ( 4, 4 ), ( 4, 0 ), ( 5, -1 ), ( 6, -2 ), ( -2, 2 ), ( -2, 0 ), ( -2, -2 ), ( -2, -2 ), ( -1, -3 ), ( 0, -4 ) ], unoccupied = [ ( 0, 4 ), ( 1, 5 ), ( 2, 6 ), ( 2, -2 ), ( 2, 0 ), ( 2, 2 ), ( 2, 2 ), ( 3, 3 ), ( 4, 4 ), ( 4, 0 ), ( 5, -1 ), ( 6, -2 ), ( -2, 2 ), ( -2, 0 ), ( -2, -2 ), ( -2, -2 ), ( -1, -3 ), ( 0, -4 ) ] } }
    , ActualPiece { moveEffect = NoEffect, controller = Player, movePattern = { occupied = [ ( 0, 0 ) ], unoccupied = [ ( 0, 0 ) ] } }
    , ActualPiece { moveEffect = Capture, controller = Player, movePattern = { occupied = [ ( 0, 0 ) ], unoccupied = [ ( 0, 2 ), ( 1, -1 ), ( 1, 1 ), ( 2, 0 ), ( -1, 1 ), ( -1, -1 ) ] } }
    , ActualPiece { moveEffect = NoEffect, controller = Computer, movePattern = { occupied = [ ( 0, 0 ) ], unoccupied = [ ( 0, 4 ), ( 1, 5 ), ( 2, 6 ), ( 2, -2 ), ( 2, 0 ), ( 2, 2 ), ( 2, 2 ), ( 3, 3 ), ( 4, 4 ), ( 4, 0 ), ( 5, -1 ), ( 6, -2 ), ( -2, 2 ), ( -2, 0 ), ( -2, -2 ), ( -2, -2 ), ( -1, -3 ), ( 0, -4 ) ] } }
    , ActualPiece { moveEffect = NoEffect, controller = Computer, movePattern = { occupied = [ ( 0, 2 ), ( 1, -1 ), ( 1, 1 ), ( 2, 0 ), ( -1, 1 ), ( -1, -1 ) ], unoccupied = [ ( 0, 0 ) ] } }
    ]
        |> List.sortBy Basics.toString



-- Pieces.actualPieceTypePossibilities
--     |> List.map Pieces.ActualPiece
--     |> (++)
--         [ NoPiece
--         , NoPiece
--         ]


defaultMoveOccupancyDeck =
    Pieces.moveOccupancyPossibilities


defaultSpaceDeck =
    [ Green
    , Green
    , Red
    , Red
    , EmptySpace
    ]
        |> List.sortBy Basics.toString



--  88888888b                                       dP   oo
--  88                                              88
-- a88aaaa    dP.  .dP 88d888b. .d8888b. 88d888b. d8888P dP 88d888b. .d8888b.
--  88         `8bd8'  88'  `88 88'  `88 88'  `88   88   88 88'  `88 88'  `88
--  88         .d88b.  88.  .88 88.  .88 88         88   88 88    88 88.  .88
--  88888888P dP'  `dP 88Y888P' `88888P' dP         dP   dP dP    dP `8888P88
--                     88                                                 .88
--                     dP                                             d8888P


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
        , ( "gameEndCons", encodeGameEndCons exportModel.gameEndCons )
        , ( "viewScale", Encode.float exportModel.viewScale )
        , ( "pieceAppearances"
          , exportModel
                |> .pieceAppearances
                |> PieceAppearances.toList
                |> encodeMap encodeAppearancePair
          )
        , ( "version", Encode.string Version.string )
        ]



-- dP                                         dP   oo
-- 88                                         88
-- 88 88d8b.d8b. 88d888b. .d8888b. 88d888b. d8888P dP 88d888b. .d8888b.
-- 88 88'`88'`88 88'  `88 88'  `88 88'  `88   88   88 88'  `88 88'  `88
-- 88 88  88  88 88.  .88 88.  .88 88         88   88 88    88 88.  .88
-- dP dP  dP  dP 88Y888P' `88888P' dP         dP   dP dP    dP `8888P88
--               88                                                 .88
--               dP                                             d8888P


parse : Decode.Value -> Result String ExportModel
parse =
    Decode.decodeValue decoder


parseString : String -> Result String ExportModel
parseString =
    Decode.decodeString decoder


parseStringDefaultingOnError : String -> Result String ExportModel
parseStringDefaultingOnError =
    Decode.decodeString lenientDecoder


decoder : Decoder ExportModel
decoder =
    ExportModel
        `Decode.map` strictGridWidth
        `apply` strictGridHeight
        `apply` strictSpaceDeck
        `apply` strictPieceDeck
        `apply` strictGameEndCons
        `apply` strictViewScale
        `apply` strictPieceAppearances


strictGridWidth =
    "gridWidth" := Decode.int


strictGridHeight =
    "gridHeight" := Decode.int


strictSpaceDeck =
    "spaceDeck" := Decode.list spaceTypeDecoder


strictPieceDeck =
    "pieceDeck" := Decode.list protoPieceDecoder


strictGameEndCons =
    "gameEndCons" := gameEndConsDecoder


strictViewScale =
    "viewScale" := Decode.float


strictPieceAppearances =
    "pieceAppearances" := pieceAppearancesDecoder


lenientDecoder : Decoder ExportModel
lenientDecoder =
    ExportModel
        `Decode.map` Decode.oneOf
                        [ strictGridWidth
                        , Decode.succeed defaultWidth
                        ]
        `apply` Decode.oneOf
                    [ strictGridHeight
                    , Decode.succeed defaultHeight
                    ]
        `apply` Decode.oneOf
                    [ strictSpaceDeck
                    , Decode.succeed defaultSpaceDeck
                    ]
        `apply` Decode.oneOf
                    [ strictPieceDeck
                    , Decode.succeed defaultPieceTypeDeck
                    ]
        `apply` Decode.oneOf
                    [ strictGameEndCons
                    , Decode.succeed defaultGameEndCons
                    ]
        `apply` Decode.oneOf
                    [ strictViewScale
                    , Decode.succeed defaultViewScale
                    ]
        `apply` Decode.oneOf
                    [ strictPieceAppearances
                    , Decode.succeed defaultPieceAppearances
                    ]


gameEndConsDecoder : Decoder GameEndCons
gameEndConsDecoder =
    gamePredicateDecoder
        |> Decode.list
        |> Decode.map
            (\list ->
                case list of
                    first :: second :: _ ->
                        GameEndCons first second

                    _ ->
                        defaultGameEndCons
            )
