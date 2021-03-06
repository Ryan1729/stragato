module CommonEncoders exposing (..)

import Json.Encode as Encode
import PieceAppearances exposing (PieceAppearances, Appearance, AppearancePair)
import Spaces exposing (Spaces, Space, SpaceType(..))
import Pieces exposing (Pieces, Piece, Shape(..), PieceType, Controller(..), MovePattern, ProtoPiece(..), MoveEffect(..))
import PosInt
import Math.Vector2 as V2 exposing (Vec2, vec2)
import GameEndCons exposing (GameEndCons(..), GamePredicate(..))


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


encodeMovePatteen : MovePattern -> Encode.Value
encodeMovePatteen movePattern =
    Encode.object
        [ ( "occupied", movePattern.occupied |> encodeMap encodeMoveOffset )
        , ( "unoccupied", movePattern.unoccupied |> encodeMap encodeMoveOffset )
        ]


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
        , ( "movePattern", pieceType.movePattern |> encodeMovePatteen )
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
    case con of
        NoPiecesControlledBy controller ->
            encodeTag "NoPiecesControlledBy" [ encodeController controller ]

        NoPiecesStrictlyControlledBy controller ->
            encodeTag "NoPiecesStrictlyControlledBy" [ encodeController controller ]

        NoPiecesOfGivenTypeCanMove pieceType ->
            encodeTag "NoPiecesOfGivenTypeCanMove" [ encodePieceType pieceType ]


encodeTag : String -> List Encode.Value -> Encode.Value
encodeTag s list =
    (Encode.string s :: list)
        |> List.indexedMap assignTag
        |> Encode.object


assignTag : Int -> Encode.Value -> ( String, Encode.Value )
assignTag index value =
    case index of
        0 ->
            ( "tag", value )

        x ->
            ( x |> Basics.toString, value )


encodeAppearancePair : ( PieceType, Appearance ) -> Encode.Value
encodeAppearancePair ( pieceType, apearance ) =
    Encode.list [ encodePieceType pieceType, encodeAppearance apearance ]


encodeAppearance : Appearance -> Encode.Value
encodeAppearance ( shape, string ) =
    Encode.list
        [ encodeShape shape
        , Encode.string string
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

        Eye ->
            Encode.string "Eye"


encodeMoveOffset ( x, y ) =
    Encode.list
        [ x
            |> Encode.int
        , y
            |> Encode.int
        ]
