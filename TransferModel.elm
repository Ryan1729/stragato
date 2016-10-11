module TransferModel exposing (..)

import ExportModel exposing (ExportModel)
import Spaces exposing (Spaces, Space, SpaceType(..))
import Pieces exposing (Pieces, Piece, Shape(..), PieceType, Controller(..), MoveType(..), ProtoPiece(..), MoveEffect(..))


type alias TransferModel =
    { exportModel : ExportModel
    , pieces : Pieces
    , spaces : List ( ( Int, Int ), Space )
    , ignoreGameResult : Bool
    , showSpaceOutlines : Bool
    , allowMovingAllPieces : Bool
    }
