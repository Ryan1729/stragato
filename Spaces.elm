module Spaces exposing (..)

import Math.Vector2 as V2 exposing (Vec2, vec2, add)
import Dict exposing (Dict)


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


getPosition : ( Int, Int ) -> Spaces -> Maybe Vec2
getPosition id spaces =
    Maybe.map .position (Dict.get id spaces)


getSpaceType : ( Int, Int ) -> Spaces -> Maybe SpaceType
getSpaceType id spaces =
    Maybe.map .spaceType (Dict.get id spaces)
