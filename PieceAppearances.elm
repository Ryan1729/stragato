module PieceAppearances exposing (..)

import Math.Vector2 exposing (Vec2)
import Pieces exposing (Controller(..), MoveType(..), MoveEffect(..), Shape(..), PieceType)
import GenericDict exposing (GenericDict)
import Extras
import PosInt
import Points


type alias Appearance =
    ( Shape, String, Icon )


type alias PieceAppearances =
    GenericDict PieceType Appearance


get : PieceType -> PieceAppearances -> Appearance
get pieceType pieceAppearances =
    GenericDict.get pieceType pieceAppearances
        --TODO more obviously wrong default case?
        |>
            Maybe.withDefault ( Eye, "#f0f", NoIcon )


comparer : PieceType -> PieceType -> Order
comparer { moveEffect, controller, moveType } other =
    case moveEffectCompare moveEffect other.moveEffect of
        EQ ->
            case controllerCompare controller other.controller of
                EQ ->
                    moveTypeCompare moveType other.moveType

                order ->
                    order

        order ->
            order


moveEffectCompare : MoveEffect -> MoveEffect -> Order
moveEffectCompare moveEffect moveEffect' =
    let
        moveEffectInt =
            getMoveEffectInt moveEffect

        moveEffectInt' =
            getMoveEffectInt moveEffect'
    in
        compare moveEffectInt moveEffectInt'


bumpOffset =
    List.length Pieces.someMoveEffectPossibilities


getMoveEffectInt moveEffect =
    case moveEffect of
        Bump posInt ->
            PosInt.toInt posInt + bumpOffset

        _ ->
            Extras.indexOfDefault Pieces.someMoveEffectPossibilities moveEffect


controllerCompare : Controller -> Controller -> Order
controllerCompare controller controller' =
    let
        controllerInt =
            Extras.indexOfDefault Pieces.controllerPossibilities controller

        controllerInt' =
            Extras.indexOfDefault Pieces.controllerPossibilities controller'
    in
        compare controllerInt controllerInt'


moveTypeCompare : MoveType -> MoveType -> Order
moveTypeCompare moveType moveType' =
    let
        moveTypeInt =
            Extras.indexOfDefault Pieces.moveTypePossibilities moveType

        moveTypeInt' =
            Extras.indexOfDefault Pieces.moveTypePossibilities moveType'
    in
        compare moveTypeInt moveTypeInt'


fromList : List ( PieceType, Appearance ) -> PieceAppearances
fromList =
    GenericDict.fromList comparer


toList =
    GenericDict.toList


update :
    PieceType
    -> (Maybe Appearance -> Maybe Appearance)
    -> PieceAppearances
    -> PieceAppearances
update =
    GenericDict.update


updatePoints :
    PieceType
    -> List Vec2
    -> PieceAppearances
    -> PieceAppearances
updatePoints pieceType list pieceAppearances =
    update pieceType
        (\maybeOldApppearance ->
            case maybeOldApppearance of
                Just ( _, colour, icon ) ->
                    Just ( PointsList list, colour, icon )

                Nothing ->
                    Just
                        ( PointsList list
                        , getFill pieceType.controller
                        , getIcon pieceType.moveType
                        )
        )
        pieceAppearances


updateColour :
    PieceType
    -> String
    -> PieceAppearances
    -> PieceAppearances
updateColour pieceType colour pieceAppearances =
    update pieceType
        (\maybeOldApppearance ->
            case maybeOldApppearance of
                Just ( points, _, icon ) ->
                    Just ( points, colour, icon )

                Nothing ->
                    Just
                        ( getShape pieceType.moveEffect
                        , colour
                        , getIcon pieceType.moveType
                        )
        )
        pieceAppearances


updateIcon :
    PieceType
    -> Icon
    -> PieceAppearances
    -> PieceAppearances
updateIcon pieceType icon pieceAppearances =
    update pieceType
        (\maybeOldApppearance ->
            case maybeOldApppearance of
                Just ( points, colour, _ ) ->
                    Just ( points, colour, icon )

                Nothing ->
                    Just
                        ( getShape pieceType.moveEffect
                        , getFill pieceType.controller
                        , icon
                        )
        )
        pieceAppearances


pairWithAppearance : PieceType -> ( PieceType, Appearance )
pairWithAppearance ({ moveEffect, controller, moveType } as pieceType) =
    ( pieceType, ( getShape moveEffect, getFill controller, getIcon moveType ) )


type Icon
    = EmptySpaceIcon
    | ShapeSpaceIcon Shape
    | ShapeIcon Shape
    | NoIcon


getIcon : MoveType -> Icon
getIcon moveType =
    case moveType of
        AnySpace ->
            NoIcon

        Occupied ->
            Points.trianglePointsList
                |> Points.pointsListToPiecePointsList
                |> PointsList
                |> ShapeSpaceIcon

        Unoccupied ->
            EmptySpaceIcon


getShape : MoveEffect -> Shape
getShape moveEffect =
    case moveEffect of
        Capture ->
            Points.pointsListToPiecePointsList Points.trianglePointsList
                |> PointsList

        Bump posInt ->
            case PosInt.toInt posInt of
                1 ->
                    Points.pointsListToPiecePointsList Points.weirdThingPointsList
                        |> PointsList

                2 ->
                    Points.pointsListToPiecePointsList Points.twistedPlusPointsList
                        |> PointsList

                _ ->
                    Points.pointsListToPiecePointsList Points.fangsPointsList
                        |> PointsList

        Swap ->
            Eye

        Copy ->
            Points.pointsListToPiecePointsList Points.petalsPointsList
                |> PointsList

        NoEffect ->
            Points.pointsListToPiecePointsList Points.starPointsList
                |> PointsList


getFill : Controller -> String
getFill control =
    case control of
        Player ->
            "#fa0"

        Computer ->
            "#0af"

        Both ->
            "#faf"

        None ->
            "#0a0"
