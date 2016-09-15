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
