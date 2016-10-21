module Fuzzers exposing (..)

import Fuzz exposing (Fuzzer)
import ExportModel exposing (ExportModel)
import Random.Pcg as Random exposing (Generator, map)
import GameEndCons
import Shrink exposing (Shrinker)
import Lazy.List exposing (LazyList)
import Pieces exposing (Pieces, Piece, ProtoPiece(..), Shape(..), Controller(..), MovePattern, PieceType, MoveEffect)
import PieceAppearances exposing (PieceAppearances, Appearance, AppearancePair)
import Math.Vector2 as V2 exposing (Vec2)
import Spaces exposing (Spaces, Space, SpaceType(..))
import GameEndCons exposing (GameEndCons(..), GamePredicate(..))
import TransferModel exposing (TransferModel)
import Dict


transferModelFuzzer : Fuzzer TransferModel
transferModelFuzzer =
    TransferModel
        `Fuzz.map` exportModelFuzzer
        `Fuzz.andMap` Fuzz.maybe piecesFuzzer
        `Fuzz.andMap` Fuzz.maybe spacesFuzzer
        `Fuzz.andMap` Fuzz.bool
        `Fuzz.andMap` Fuzz.bool
        `Fuzz.andMap` Fuzz.bool


piecesFuzzer : Fuzzer Pieces
piecesFuzzer =
    piecePairFuzzer |> Fuzz.list |> Fuzz.map Dict.fromList


piecePairFuzzer : Fuzzer ( Int, Piece )
piecePairFuzzer =
    Fuzz.tuple ( Fuzz.int, pieceFuzzer )


pieceFuzzer : Fuzzer Piece
pieceFuzzer =
    Fuzz.map2 Piece
        pieceTypeFuzzer
        vec2Fuzzer


spacesFuzzer : Fuzzer Spaces
spacesFuzzer =
    spacePairFuzzer |> Fuzz.list |> Fuzz.map Dict.fromList


spacePairFuzzer : Fuzzer ( ( Int, Int ), Space )
spacePairFuzzer =
    Fuzz.tuple ( intTupleFuzzer, spaceFuzzer )


spaceFuzzer : Fuzzer Space
spaceFuzzer =
    Fuzz.map2 Space
        vec2Fuzzer
        spaceTypeFuzzer


exportModelFuzzer : Fuzzer ExportModel
exportModelFuzzer =
    ExportModel
        `Fuzz.map` Fuzz.int
        `Fuzz.andMap` Fuzz.int
        `Fuzz.andMap` Fuzz.list spaceTypeFuzzer
        `Fuzz.andMap` Fuzz.list protoPieceFuzzer
        `Fuzz.andMap` gameEndConsFuzzer
        `Fuzz.andMap` Fuzz.float
        `Fuzz.andMap` pieceAppearancesFuzzer


gameEndConsFuzzer : Fuzzer GameEndCons
gameEndConsFuzzer =
    Fuzz.custom gameEndConsGenerator gameEndConsShrinker


gameEndConsGenerator : Generator GameEndCons
gameEndConsGenerator =
    Random.map2 GameEndCons.GameEndCons gamePredicateGenerator gamePredicateGenerator


gameEndConsShrinker : Shrinker GameEndCons
gameEndConsShrinker =
    GameEndCons (NoPiecesControlledBy Player) (NoPiecesControlledBy Computer)
        |> shrikerWithDefault


shrikerWithDefault : a -> Shrinker a
shrikerWithDefault defaultThing thing =
    if thing == defaultThing then
        Lazy.List.empty
    else
        Lazy.List.fromList [ defaultThing ]


gamePredicateGenerator : Generator GamePredicate
gamePredicateGenerator =
    Random.choices
        [ Random.sample GameEndCons.noPiecesControlledByPossibilities
            |> (NoPiecesControlledBy Player
                    |> Maybe.withDefault
                    |> map
               )
        , Random.sample GameEndCons.noPiecesStrictlyControlledByPossibilities
            |> (NoPiecesStrictlyControlledBy Player
                    |> Maybe.withDefault
                    |> map
               )
        , Random.sample GameEndCons.noPiecesOfGivenTypeCanMovePossibilities
            |> (--defaulting to another tpye of GamePredicate because piece literals are inconvenient
                NoPiecesStrictlyControlledBy Player
                    |> Maybe.withDefault
                    |> map
               )
        ]


spaceTypeFuzzer : Fuzzer SpaceType
spaceTypeFuzzer =
    Fuzz.custom (Random.sample Spaces.spaceTypePossibilities |> map (Maybe.withDefault EmptySpace)) (shrikerWithDefault EmptySpace)


protoPieceFuzzer : Fuzzer ProtoPiece
protoPieceFuzzer =
    Fuzz.custom (Random.sample Pieces.protoPiecePossibilities |> map (Maybe.withDefault NoPiece)) (shrikerWithDefault NoPiece)


pieceAppearancesFuzzer : Fuzzer PieceAppearances
pieceAppearancesFuzzer =
    appearancePairFuzzer |> Fuzz.list |> Fuzz.map PieceAppearances.fromList


appearancePairFuzzer : Fuzzer AppearancePair
appearancePairFuzzer =
    Fuzz.tuple ( pieceTypeFuzzer, appearanceFuzzer )


pieceTypeFuzzer : Fuzzer PieceType
pieceTypeFuzzer =
    Fuzz.map3 PieceType
        moveEffectFuzzer
        controllerFuzzer
        movePatternFuzzer


moveEffectFuzzer : Fuzzer MoveEffect
moveEffectFuzzer =
    uniformlyWeightedListOrCrash Pieces.someMoveEffectPossibilities


controllerFuzzer : Fuzzer Controller
controllerFuzzer =
    uniformlyWeightedListOrCrash Pieces.controllerPossibilities


movePatternFuzzer : Fuzzer MovePattern
movePatternFuzzer =
    MovePattern
        `Fuzz.map` intTupleListFuzzer
        `Fuzz.andMap` intTupleListFuzzer


intTupleListFuzzer : Fuzzer (List ( Int, Int ))
intTupleListFuzzer =
    Fuzz.list intTupleFuzzer


intTupleFuzzer : Fuzzer ( Int, Int )
intTupleFuzzer =
    ( Fuzz.int, Fuzz.int )
        |> Fuzz.tuple


uniformlyWeightedListOrCrash : List a -> Fuzzer a
uniformlyWeightedListOrCrash =
    List.map (\x -> ( 1, Fuzz.constant x ))
        >> Fuzz.frequencyOrCrash


appearanceFuzzer : Fuzzer Appearance
appearanceFuzzer =
    Fuzz.tuple ( shapeFuzzer, Fuzz.string )


shapeFuzzer : Fuzzer Shape
shapeFuzzer =
    Fuzz.frequencyOrCrash [ ( 3, pointsListFuzzer ), ( 1, Fuzz.constant Eye ) ]


pointsListFuzzer : Fuzzer Shape
pointsListFuzzer =
    vec2Fuzzer
        |> Fuzz.list
        |> Fuzz.map PointsList


vec2Fuzzer : Fuzzer Vec2
vec2Fuzzer =
    ( Fuzz.float, Fuzz.float )
        |> Fuzz.tuple
        |> Fuzz.map V2.fromTuple
