module TransferModel exposing (..)

import ExportModel exposing (ExportModel)
import Math.Vector2 as V2 exposing (Vec2, vec2)
import Spaces exposing (Spaces, Space, SpaceType(..))
import Pieces exposing (Pieces, Piece, Shape(..), PieceType, Controller(..), MoveType(..), ProtoPiece(..), MoveEffect(..))
import Extras
import Dict
import Json.Decode as Decode exposing (Decoder, (:=))
import CommonDecoders exposing (apply)


type alias TransferModel =
    { exportModel : ExportModel
    , pieces : Pieces
    , spaces : Spaces
    , ignoreGameResult : Bool
    , showSpaceOutlines : Bool
    , allowMovingAllPieces : Bool
    }



-- dP                                         dP   oo
-- 88                                         88
-- 88 88d8b.d8b. 88d888b. .d8888b. 88d888b. d8888P dP 88d888b. .d8888b.
-- 88 88'`88'`88 88'  `88 88'  `88 88'  `88   88   88 88'  `88 88'  `88
-- 88 88  88  88 88.  .88 88.  .88 88         88   88 88    88 88.  .88
-- dP dP  dP  dP 88Y888P' `88888P' dP         dP   dP dP    dP `8888P88
--               88                                                 .88
--               dP                                             d8888P


parse : Decode.Value -> Result String TransferModel
parse =
    Decode.decodeValue decoder


decoder : Decoder TransferModel
decoder =
    TransferModel
        `Decode.map` ExportModel.lenientDecoder
        `apply` piecesDecoder
        `apply` spacesDecoder
        `apply` ("ignoreGameResult" := Decode.bool)
        `apply` ("showSpaceOutlines" := Decode.bool)
        `apply` ("allowMovingAllPieces" := Decode.bool)


piecesDecoder : Decoder Pieces
piecesDecoder =
    ("pieces" := Decode.list piecePairDecoder)
        |> Decode.map Dict.fromList


piecePairDecoder : Decoder ( Int, Piece )
piecePairDecoder =
    Decode.tuple2 (,) Decode.int pieceDecoder


pieceDecoder : Decoder Piece
pieceDecoder =
    Decode.object2 Piece CommonDecoders.pieceTypeDecoder positionDecoder


spacesDecoder : Decoder Spaces
spacesDecoder =
    ("spaces" := Decode.list spacePairDecoder)
        |> Decode.map Dict.fromList


spacePairDecoder : Decoder ( ( Int, Int ), Space )
spacePairDecoder =
    Decode.tuple2 (,) spaceIndexDecoder spaceDecoder


spaceIndexDecoder : Decoder ( Int, Int )
spaceIndexDecoder =
    Decode.tuple2 (,) Decode.int Decode.int


spaceDecoder : Decoder Space
spaceDecoder =
    Decode.object2 Space positionDecoder CommonDecoders.spaceTypeDecoder


positionDecoder : Decoder Vec2
positionDecoder =
    "position" := CommonDecoders.vec2Decoder


type alias TransferSpaces =
    List ( ( Int, Int ), TransferSpace )


type alias TransferSpace =
    { position : ( Float, Float ), spaceType : String }


spacesToTransferSpaces : Spaces -> TransferSpaces
spacesToTransferSpaces =
    Dict.toList
        >> List.map (Extras.mapSnd spaceToTransferSpace)


transferSpacesToSpaces : TransferSpaces -> Spaces
transferSpacesToSpaces =
    List.map (Extras.mapSnd transferSpaceToSpace)
        >> Dict.fromList


spaceToTransferSpace : Space -> TransferSpace
spaceToTransferSpace space =
    { position = V2.toTuple space.position
    , spaceType = spaceTypeToString space.spaceType
    }


transferSpaceToSpace : TransferSpace -> Space
transferSpaceToSpace transferSpace =
    { position = V2.fromTuple transferSpace.position
    , spaceType = stringToSpaceType transferSpace.spaceType
    }


spaceTypeToString : SpaceType -> String
spaceTypeToString spaceType =
    case spaceType of
        Green ->
            "Green"

        Red ->
            "Red"

        Yellow ->
            "Yellow"

        EmptySpace ->
            "EmptySpace"


stringToSpaceType : String -> SpaceType
stringToSpaceType string =
    case string of
        "Green" ->
            Green

        "Red" ->
            Red

        "Yellow" ->
            Yellow

        _ ->
            EmptySpace
