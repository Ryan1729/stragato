module ExportModel exposing (..)

import GameEndCons exposing (GameEndCons(..), GamePredicate(..))
import PieceAppearances exposing (PieceAppearances, Appearance, Icon(..), AppearancePair)
import Spaces exposing (Spaces, Space, SpaceType(..))
import Pieces exposing (Pieces, Piece, Shape(..), PieceType, Controller(..), MoveType(..), ProtoPiece(..), MoveEffect(..))
import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder, (:=))
import PosInt
import String
import CommonDecoders exposing (..)
import CommonEncoders exposing (..)
import Math.Vector2 as V2 exposing (Vec2, vec2)


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
