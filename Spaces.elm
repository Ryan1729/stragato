module Spaces exposing (..)

import Math.Vector2 as V2 exposing (Vec2, vec2, add)
import Dict exposing (Dict)
import Extras


type alias SpaceIndex =
    ( Int, Int )


type alias Spaces =
    Dict SpaceIndex Space


type alias Space =
    { position : Vec2
    , spaceType : SpaceType
    }


type SpaceType
    = Green
    | Red
    | Yellow
    | EmptySpace


spaceTypePossibilities =
    [ Green, Red, Yellow, EmptySpace ]


isActualSpace space =
    space.spaceType /= EmptySpace


getPosition : SpaceIndex -> Spaces -> Maybe Vec2
getPosition id spaces =
    Maybe.map .position (Dict.get id spaces)


getSpaceType : SpaceIndex -> Spaces -> Maybe SpaceType
getSpaceType id spaces =
    Maybe.map .spaceType (Dict.get id spaces)


getActualSpaces : Spaces -> Spaces
getActualSpaces spaces =
    Dict.filter (Extras.ignoreFirstArg isActualSpace) spaces


indexIsOfActualSpace : Spaces -> SpaceIndex -> Bool
indexIsOfActualSpace spaces index =
    getActualSpaces spaces
        |> Dict.keys
        |> List.member index


positionIsOnActualSpace spaces targetSpacePosition =
    List.member targetSpacePosition <| getActualSpacePositions spaces


getActualSpacePositions : Spaces -> List Vec2
getActualSpacePositions spaces =
    Dict.values spaces
        |> List.filterMap
            (\space ->
                if isActualSpace space then
                    Just space.position
                else
                    Nothing
            )


getSpaceFromPosition : Spaces -> Vec2 -> Maybe SpaceIndex
getSpaceFromPosition spaces targetPosition =
    spaces
        |> Dict.toList
        |> List.filterMap
            (\( index, space ) ->
                --TODO epsilon here?
                if space.position == targetPosition then
                    Just index
                else
                    Nothing
            )
        |> List.head


getNonMatchingSpaceIndicies : Spaces -> Vec2 -> List SpaceIndex
getNonMatchingSpaceIndicies spaces position =
    case getSpaceFromPosition spaces position of
        Nothing ->
            Dict.keys spaces

        Just spaceIndex ->
            Dict.keys spaces
                |> List.filter ((/=) spaceIndex)


getIndexOffsetFromPosition : Spaces -> SpaceIndex -> Vec2 -> Maybe ( Int, Int )
getIndexOffsetFromPosition spaces ( tx, ty ) piecePosition =
    getSpaceFromPosition spaces piecePosition
        |> Maybe.map (\( sx, sy ) -> ( tx - sx, ty - sy ))
