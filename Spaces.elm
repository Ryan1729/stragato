module Spaces exposing (..)

import Math.Vector2 as V2 exposing (Vec2, vec2, add)
import Dict exposing (Dict)
import Extras


type alias Spaces =
    Dict ( Int, Int ) Space


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


getPosition : ( Int, Int ) -> Spaces -> Maybe Vec2
getPosition id spaces =
    Maybe.map .position (Dict.get id spaces)


getSpaceType : ( Int, Int ) -> Spaces -> Maybe SpaceType
getSpaceType id spaces =
    Maybe.map .spaceType (Dict.get id spaces)


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


getSpaceFromPosition : Spaces -> Vec2 -> Maybe ( Int, Int )
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
