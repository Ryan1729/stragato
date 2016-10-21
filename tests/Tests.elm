module Tests exposing (..)

import Test exposing (..)
import Expect
import String
import Fuzzers
import ExportModel
import TransferModel
import Json.Decode as Decode
import CommonDecoders
import CommonEncoders
import Pieces exposing (..)
import GameEndCons exposing (..)


all : Test
all =
    describe "Import/Export"
        [ fuzz Fuzzers.exportModelFuzzer "ExportModel is unchanged by exporting then importing"
            <| \exportModel ->
                exportModel
                    |> ExportModel.encode
                    |> ExportModel.parse
                    |> Expect.equal (Ok exportModel)
        , fuzz Fuzzers.transferModelFuzzer "ExportModel is unchanged by exporting then importing"
            <| \transferModel ->
                transferModel
                    |> TransferModel.encode
                    |> TransferModel.parse
                    |> Expect.equal (Ok transferModel)
        , test "can decode problematic movepatterrn"
            <| \() ->
                let
                    movePattern =
                        { occupied =
                            [ ( 0, 4 )
                            ]
                        , unoccupied =
                            [ ( 0, 2 )
                            ]
                        }
                in
                    movePattern
                        |> CommonEncoders.encodeMovePatteen
                        |> Decode.decodeValue CommonDecoders.movePatternDecoder
                        |> Expect.equal (Ok movePattern)
        , test "can decode problematic GameEndCons with piecetype in it"
            <| \() ->
                let
                    endCons =
                        GameEndCons
                            (NoPiecesOfGivenTypeCanMove
                                { moveEffect = Capture
                                , controller = Computer
                                , movePattern =
                                    { occupied =
                                        [ ( 0, 2 ), ( 1, -1 ), ( 1, 1 ), ( 2, 0 ), ( -1, 1 ), ( -1, -1 ) ]
                                    , unoccupied = [ ( 0, 2 ), ( 1, -1 ), ( 1, 1 ), ( 2, 0 ), ( -1, 1 ), ( -1, -1 ) ]
                                    }
                                }
                            )
                            (NoPiecesControlledBy Player)
                in
                    endCons
                        |> CommonEncoders.encodeGameEndCons
                        |> Decode.decodeValue ExportModel.gameEndConsDecoder
                        |> Expect.equal (Ok endCons)
        , test "can decode problematic GamePredicate with piecetype in it"
            <| \() ->
                let
                    pred =
                        (NoPiecesOfGivenTypeCanMove
                            { moveEffect = Capture
                            , controller = Computer
                            , movePattern =
                                { occupied =
                                    [ ( 0, 2 ), ( 1, -1 ), ( 1, 1 ), ( 2, 0 ), ( -1, 1 ), ( -1, -1 ) ]
                                , unoccupied = [ ( 0, 2 ), ( 1, -1 ), ( 1, 1 ), ( 2, 0 ), ( -1, 1 ), ( -1, -1 ) ]
                                }
                            }
                        )
                in
                    pred
                        |> CommonEncoders.encodeGamePredicate
                        |> Decode.decodeValue CommonDecoders.gamePredicateDecoder
                        |> Expect.equal (Ok pred)
        , test "can decode problematic piecetype"
            <| \() ->
                let
                    pt =
                        { moveEffect = Capture
                        , controller = Computer
                        , movePattern =
                            { occupied =
                                [ ( 0, 2 ), ( 1, -1 ), ( 1, 1 ), ( 2, 0 ), ( -1, 1 ), ( -1, -1 ) ]
                            , unoccupied = [ ( 0, 2 ), ( 1, -1 ), ( 1, 1 ), ( 2, 0 ), ( -1, 1 ), ( -1, -1 ) ]
                            }
                        }
                in
                    pt
                        |> CommonEncoders.encodePieceType
                        |> Decode.decodeValue CommonDecoders.pieceTypeDecoder
                        |> Expect.equal (Ok pt)
        ]
