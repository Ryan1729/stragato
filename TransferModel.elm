module TransferModel exposing (..)

import ExportModel exposing (ExportModel)
import Math.Vector2 as V2 exposing (Vec2, vec2)
import Spaces exposing (Spaces, Space, SpaceType(..))
import Pieces exposing (Pieces, Piece, Shape(..), PieceType, Controller(..), MoveOccupancy(..), ProtoPiece(..), MoveEffect(..))
import Extras
import Dict
import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder, (:=))
import CommonEncoders exposing (encodeMap)
import CommonDecoders exposing (apply)


type alias TransferModel =
    { exportModel : ExportModel
    , pieces : Maybe Pieces
    , spaces : Maybe Spaces
    , ignoreGameResult : Bool
    , showSpaceOutlines : Bool
    , allowMovingAllPieces : Bool
    }



--  88888888b                                       dP   oo
--  88                                              88
-- a88aaaa    dP.  .dP 88d888b. .d8888b. 88d888b. d8888P dP 88d888b. .d8888b.
--  88         `8bd8'  88'  `88 88'  `88 88'  `88   88   88 88'  `88 88'  `88
--  88         .d88b.  88.  .88 88.  .88 88         88   88 88    88 88.  .88
--  88888888P dP'  `dP 88Y888P' `88888P' dP         dP   dP dP    dP `8888P88
--                     88                                                 .88
--                     dP                                             d8888P


toString : TransferModel -> String
toString transferModel =
    transferModel
        |> encode
        |> Encode.encode 4


encode : TransferModel -> Encode.Value
encode transferModel =
    Encode.object
        [ ( "exportModel", ExportModel.encode transferModel.exportModel )
        , ( "pieces"
          , transferModel
                |> .pieces
                |> Maybe.map
                    (Dict.toList
                        >> encodeMap encodePiecePair
                    )
                |> Maybe.withDefault Encode.null
          )
        , ( "spaces"
          , transferModel
                |> .spaces
                |> Maybe.map
                    (Dict.toList
                        >> encodeMap encodeSpacePair
                    )
                |> Maybe.withDefault Encode.null
          )
        , ( "ignoreGameResult", Encode.bool transferModel.ignoreGameResult )
        , ( "showSpaceOutlines", Encode.bool transferModel.showSpaceOutlines )
        , ( "allowMovingAllPieces", Encode.bool transferModel.allowMovingAllPieces )
        ]


encodePiecePair : ( Int, Piece ) -> Encode.Value
encodePiecePair ( index, piece ) =
    Encode.list [ Encode.int index, encodePiece piece ]


encodePiece : Piece -> Encode.Value
encodePiece piece =
    Encode.object
        [ ( "pieceType", CommonEncoders.encodePieceType piece.pieceType )
        , ( "position", CommonEncoders.encodeVec2 piece.position )
        ]


encodeSpacePair : ( ( Int, Int ), Space ) -> Encode.Value
encodeSpacePair ( index, space ) =
    Encode.list [ encodeSpaceIndex index, encodeSpace space ]


encodeSpaceIndex : ( Int, Int ) -> Encode.Value
encodeSpaceIndex ( x, y ) =
    Encode.list [ Encode.int x, Encode.int y ]


encodeSpace : Space -> Encode.Value
encodeSpace space =
    Encode.object
        [ ( "spaceType", CommonEncoders.encodeSpaceType space.spaceType )
        , ( "position", CommonEncoders.encodeVec2 space.position )
        ]



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
        `Decode.map` ("exportModel" := ExportModel.lenientDecoder)
        `apply` Decode.maybe piecesDecoder
        `apply` Decode.maybe spacesDecoder
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
    Decode.object2 Piece ("pieceType" := CommonDecoders.pieceTypeDecoder) positionDecoder


spacesDecoder : Decoder Spaces
spacesDecoder =
    ("spaces" := Decode.list spacePairDecoder)
        |> Decode.map Dict.fromList


spacePairDecoder : Decoder ( ( Int, Int ), Space )
spacePairDecoder =
    Decode.tuple2 (,) CommonDecoders.intPairDecoder spaceDecoder


spaceDecoder : Decoder Space
spaceDecoder =
    Decode.object2 Space positionDecoder ("spaceType" := CommonDecoders.spaceTypeDecoder)


positionDecoder : Decoder Vec2
positionDecoder =
    "position" := CommonDecoders.vec2Decoder
