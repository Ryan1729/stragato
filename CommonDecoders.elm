module CommonDecoders exposing (..)

import GameEndCons exposing (GameEndCons(..), GamePredicate(..))
import PieceAppearances exposing (PieceAppearances, Appearance, Icon(..), AppearancePair)
import Spaces exposing (Spaces, Space, SpaceType(..))
import Pieces exposing (Pieces, Piece, Shape(..), PieceType, Controller(..), MoveOccupancy(..), ProtoPiece(..), MoveEffect(..))
import Math.Vector2 as V2 exposing (Vec2, vec2)
import Json.Decode as Decode exposing (Decoder, (:=))
import PosInt
import String


--derived from
-- http://package.elm-lang.org/packages/circuithub/elm-json-extra/latest/Json-Decode-Extra


apply : Decoder (a -> b) -> Decoder a -> Decoder b
apply f aDecoder =
    f `Decode.andThen` (\f' -> f' `Decode.map` aDecoder)


spaceTypeDecoder : Decoder SpaceType
spaceTypeDecoder =
    Decode.map stringToSpaceType
        Decode.string


stringToSpaceType : String -> SpaceType
stringToSpaceType s =
    case String.toLower s of
        "green" ->
            Green

        "red" ->
            Red

        "yellow" ->
            Yellow

        _ ->
            EmptySpace


protoPieceDecoder : Decoder ProtoPiece
protoPieceDecoder =
    Decode.oneOf
        [ Decode.map ActualPiece pieceTypeDecoder
        , Decode.succeed NoPiece
        ]


pieceTypeDecoder : Decoder PieceType
pieceTypeDecoder =
    Decode.object3 PieceType
        ("moveEffect" := moveEffectDecoder)
        ("controller" := controllerDecoder)
        ("moveOccupancy" := moveOccupancyDecoder)


moveEffectDecoder : Decoder MoveEffect
moveEffectDecoder =
    Decode.map stringToMoveEffect
        Decode.string


stringToMoveEffect : String -> MoveEffect
stringToMoveEffect s =
    let
        list =
            s
                |> String.toLower
                |> String.split " "

        prefix =
            List.head list
                |> Maybe.withDefault "NoEffect"
    in
        case prefix of
            "capture" ->
                Capture

            "bump" ->
                List.tail list
                    `Maybe.andThen` List.head
                    `Maybe.andThen` parseBump
                    |> Maybe.withDefault (Bump (PosInt.fromInt 1))

            "swap" ->
                Swap

            "copy" ->
                Copy

            _ ->
                NoEffect


parseBump : String -> Maybe MoveEffect
parseBump string =
    case String.toInt string of
        Ok integer ->
            integer
                |> PosInt.fromInt
                |> Bump
                |> Just

        Err _ ->
            Nothing


controllerDecoder : Decoder Controller
controllerDecoder =
    Decode.map stringToController
        Decode.string


stringToController : String -> Controller
stringToController s =
    case String.toLower s of
        "player" ->
            Player

        "computer" ->
            Computer

        "both" ->
            Both

        _ ->
            None


moveOccupancyDecoder : Decoder MoveOccupancy
moveOccupancyDecoder =
    Decode.map stringToMoveOccupancy
        Decode.string


stringToMoveOccupancy : String -> MoveOccupancy
stringToMoveOccupancy s =
    case String.toLower s of
        "occupied" ->
            Occupied

        "unoccupied" ->
            Unoccupied

        _ ->
            AnySpace


gamePredicateDecoder : Decoder GamePredicate
gamePredicateDecoder =
    tagDecode gamePredicateInfo


tagDecode : (String -> Decoder a) -> Decoder a
tagDecode =
    Decode.andThen ("tag" := Decode.string)


gamePredicateInfo : String -> Decoder GamePredicate
gamePredicateInfo tag =
    case String.toLower tag of
        "nopiecescontrolledby" ->
            Decode.object1 NoPiecesControlledBy ("1" := controllerDecoder)

        "nopiecesstrictlycontrolledby" ->
            Decode.object1 NoPiecesStrictlyControlledBy ("1" := controllerDecoder)

        "nopiecesofgiventypecanmove" ->
            Decode.object1 NoPiecesOfGivenTypeCanMove ("1" := pieceTypeDecoder)

        _ ->
            Decode.fail "Unknown game predicate type"


pieceAppearancesDecoder : Decoder PieceAppearances
pieceAppearancesDecoder =
    appearancePairDecoder |> Decode.list |> Decode.map PieceAppearances.fromList


appearancePairDecoder : Decoder AppearancePair
appearancePairDecoder =
    Decode.tuple2 (,) pieceTypeDecoder appearanceDecoder


appearanceDecoder : Decoder Appearance
appearanceDecoder =
    Decode.tuple3 (,,) shapeDecoder Decode.string iconDecoder


iconDecoder : Decoder Icon
iconDecoder =
    Decode.oneOf
        [ simpleIconDecoder
        , taggedIconDecoder
        ]


simpleIconDecoder : Decoder Icon
simpleIconDecoder =
    Decode.string
        `Decode.andThen` (\string ->
                            case String.toLower string of
                                "emptyspaceicon" ->
                                    Decode.succeed EmptySpaceIcon

                                "noicon" ->
                                    Decode.succeed NoIcon

                                _ ->
                                    Decode.fail "Unknown simple shape type"
                         )


taggedIconDecoder : Decoder Icon
taggedIconDecoder =
    tagDecode iconInfo


iconInfo : String -> Decoder Icon
iconInfo tag =
    case String.toLower tag of
        "shapespaceicon" ->
            Decode.object1 ShapeSpaceIcon ("1" := shapeDecoder)

        "shapeicon" ->
            Decode.object1 ShapeIcon ("1" := shapeDecoder)

        _ ->
            Decode.fail "Unknown shape type"


shapeDecoder : Decoder Shape
shapeDecoder =
    Decode.oneOf [ simpleShapeDecoder, taggedShapeDecoder ]


taggedShapeDecoder : Decoder Shape
taggedShapeDecoder =
    tagDecode shapeInfo


shapeInfo : String -> Decoder Shape
shapeInfo tag =
    case String.toLower tag of
        "pointslist" ->
            Decode.object1 PointsList ("1" := pointsListDecoder)

        _ ->
            "Unknown tagged shape type: "
                ++ tag
                |> Decode.fail


pointsListDecoder : Decoder (List Vec2)
pointsListDecoder =
    vec2Decoder
        |> Decode.list


simpleShapeDecoder : Decoder Shape
simpleShapeDecoder =
    Decode.string
        `Decode.andThen` (\string ->
                            case String.toLower string of
                                "eye" ->
                                    Decode.succeed Eye

                                _ ->
                                    Decode.fail "Unknown simple shape type"
                         )


vec2Decoder : Decoder Vec2
vec2Decoder =
    Decode.tuple2 (,) Decode.float Decode.float
        |> Decode.map V2.fromTuple
