module ExportModel exposing (..)

import GameEndCons exposing (GameEndCons(..), GamePredicate(..))
import PieceAppearances exposing (PieceAppearances, Appearance, Icon(..))
import Spaces exposing (Spaces, Space, SpaceType(..))
import Pieces exposing (Pieces, Piece, Shape(..), PieceType, Controller(..), MoveType(..), ProtoPiece(..), MoveEffect(..))
import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder, (:=))
import Math.Vector2 as V2 exposing (Vec2, vec2)
import PosInt
import String


type alias ExportModel =
    { gridWidth : Int
    , gridHeight : Int
    , spaceDeck : List SpaceType
    , pieceDeck : List ProtoPiece
    , moveTypeDeck : List MoveType
    , gameEndCons : GameEndCons
    , viewScale : Float
    , pieceAppearances : PieceAppearances
    }


defaultExportModel =
    { gridWidth = defaultWidth
    , gridHeight = defaultHeight
    , spaceDeck = defaultSpaceDeck
    , pieceDeck = defaultPieceTypeDeck
    , moveTypeDeck = defaultMoveTypeDeck
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
        , ( "moveTypeDeck", encodeMap encodeMoveType exportModel.moveTypeDeck )
        , ( "gameEndCons", encodeGameEndCons exportModel.gameEndCons )
        , ( "viewScale", Encode.float exportModel.viewScale )
        , ( "pieceAppearances"
          , exportModel
                |> .pieceAppearances
                |> PieceAppearances.toList
                |> encodeMap encodeAppearancePair
          )
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
            encodePieceType pieceType

        NoPiece ->
            Encode.string "NoPiece"


encodePieceType : PieceType -> Encode.Value
encodePieceType pieceType =
    Encode.object
        [ ( "moveEffect", pieceType.moveEffect |> encodeMoveEffect )
        , ( "controller", pieceType.controller |> encodeController )
        , ( "moveType", pieceType.moveType |> encodeMoveType )
        ]


encodeController =
    stringIt


encodeGameEndCons : GameEndCons -> Encode.Value
encodeGameEndCons cons =
    case cons of
        GameEndCons winCon lossCon ->
            [ winCon, lossCon ]
                |> encodeMap encodeGamePredicate


encodeGamePredicate : GamePredicate -> Encode.Value
encodeGamePredicate con =
    Encode.list
        <| case con of
            NoPiecesControlledBy controller ->
                encodeTag "NoPiecesControlledBy" [ encodeController controller ]

            NoPiecesStrictlyControlledBy controller ->
                encodeTag "NoPiecesStrictlyControlledBy" [ encodeController controller ]

            NoPiecesOfGivenTypeCanMove pieceType ->
                encodeTag "NoPiecesStrictlyControlledBy" [ encodePieceType pieceType ]


encodeTag : String -> List Encode.Value -> List Encode.Value
encodeTag s list =
    Encode.string s :: list


encodeAppearancePair : ( PieceType, Appearance ) -> Encode.Value
encodeAppearancePair ( pieceType, apearance ) =
    Encode.list [ encodePieceType pieceType, encodeAppearance apearance ]


encodeAppearance : Appearance -> Encode.Value
encodeAppearance ( shape, string, icon ) =
    Encode.list
        [ encodeShape shape
        , Encode.string string
        , encodeIcon icon
        ]


encodeVec2 : Vec2 -> Encode.Value
encodeVec2 vector =
    Encode.list
        [ vector
            |> V2.getX
            |> Encode.float
        , vector
            |> V2.getY
            |> Encode.float
        ]


encodeShape shape =
    case shape of
        PointsList list ->
            [ encodeMap encodeVec2 list ]
                |> encodeTag "PointsList"
                |> Encode.list

        Eye ->
            Encode.string "Eye"


encodeIcon : Icon -> Encode.Value
encodeIcon icon =
    case icon of
        EmptySpaceIcon ->
            Encode.string "EmptySpaceIcon"

        ShapeSpaceIcon shape ->
            [ encodeShape shape ]
                |> encodeTag "ShapeSpaceIcon"
                |> Encode.list

        ShapeIcon shape ->
            [ encodeShape shape ]
                |> encodeTag "ShapeIcon"
                |> Encode.list

        NoIcon ->
            Encode.string "NoIcon"



-- dP                                         dP   oo
-- 88                                         88
-- 88 88d8b.d8b. 88d888b. .d8888b. 88d888b. d8888P dP 88d888b. .d8888b.
-- 88 88'`88'`88 88'  `88 88'  `88 88'  `88   88   88 88'  `88 88'  `88
-- 88 88  88  88 88.  .88 88.  .88 88         88   88 88    88 88.  .88
-- dP dP  dP  dP 88Y888P' `88888P' dP         dP   dP dP    dP `8888P88
--               88                                                 .88
--               dP                                             d8888P


parse : String -> Result String ExportModel
parse =
    Decode.decodeString decoder



--derived from
-- http://package.elm-lang.org/packages/circuithub/elm-json-extra/latest/Json-Decode-Extra


apply : Decoder (a -> b) -> Decoder a -> Decoder b
apply f aDecoder =
    f `Decode.andThen` (\f' -> f' `Decode.map` aDecoder)


decoder : Decoder ExportModel
decoder =
    ExportModel
        `Decode.map` ("gridWidth" := Decode.int)
        `apply` ("gridHeight" := Decode.int)
        `apply` ("spaceDeck" := Decode.list spaceTypeDecoder)
        `apply` ("pieceDeck" := Decode.list protoPieceDecoder)
        `apply` ("moveTypeDeck" := Decode.list moveTypeDecoder)
        `apply` ("gameEndCons" := gameEndConsDecoder)
        `apply` ("viewScale" := Decode.float)
        `apply` (Decode.oneOf
                    [ "pieceAppearances" := pieceAppearancesDecoder
                    , Decode.succeed defaultPieceAppearances
                    ]
                )


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
        ("moveType" := moveTypeDecoder)


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
                |> Debug.log "list"

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


moveTypeDecoder : Decoder MoveType
moveTypeDecoder =
    Decode.map stringToMoveType
        Decode.string


stringToMoveType : String -> MoveType
stringToMoveType s =
    case String.toLower s of
        "occupied" ->
            Occupied

        "unoccupied" ->
            Unoccupied

        _ ->
            AnySpace


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


gamePredicateDecoder : Decoder GamePredicate
gamePredicateDecoder =
    Decode.fail "Todo"



--     Decode.oneOf
--         [ gamePredicateControllerDecoder
--         , gamePredicatePieceTypeDecoder
--         ]
--
--
-- gamePredicatePieceTypeDecoder : Decoder GamePredicate
-- gamePredicatePieceTypeDecoder =
--     Decode.value
--         |> Decode.tuple2
--         |> Decode.map
--             (\( tag, value ) ->
--                 case ( String.toLower tag, value ) of
--                     ( "nopiecesofgiventypecanmove", potentialPiece ) ->
--                         case Decode.decodeValue pieceTypeDecoder potentialPiece of
--                             Ok pieceType ->
--                                 NoPiecesOfGivenTypeCanMove pieceType
--
--                             Err error ->
--                                 "bad pieceType: "
--                                     ++ error
--                                     |> Decode.fail
--
--                     _ ->
--                         Decode.fail "Unknown Controller predicate type"
--             )
--
--
-- gamePredicateControllerDecoder : Decoder GamePredicate
-- gamePredicateControllerDecoder =
--     Decode.string
--         |> Decode.list
--         |> Decode.map
--             (\list ->
--                 case List.map String.toLower list of
--                     "nopiecescontrolledby" :: controllerString :: _ ->
--                         controllerString
--                             |> stringToController
--                             |> NoPiecesControlledBy
--
--                     "nopiecesstrictlycontrolledby" :: controllerString :: _ ->
--                         controllerString
--                             |> stringToController
--                             |> NoPiecesStrictlyControlledBy
--
--                     _ ->
--                         Decode.fail "Unknown Controller predicate type"
--             )


pieceAppearancesDecoder : Decoder PieceAppearances
pieceAppearancesDecoder =
    Decode.fail "Todo"
