module Extras exposing (..)

import Dict exposing (Dict)


{-| Remove the first occurrence of a value from a list.
 from elm-community/list-extra
-}
remove : a -> List a -> List a
remove x xs =
    case xs of
        [] ->
            []

        y :: ys ->
            if x == y then
                ys
            else
                y :: remove x ys


andThen : List a -> (a -> List b) -> List b
andThen =
    flip List.concatMap



{- modified from the https://github.com/elm-community/list-extra version
   instead of returning a maybe,
    it returns the input list
-}


setAt : Int -> a -> List a -> List a
setAt index value l =
    if index < 0 then
        l
    else
        let
            head =
                List.take index l

            tail =
                List.drop index l |> List.tail
        in
            case tail of
                Nothing ->
                    l

                Just t ->
                    value :: t |> List.append head



{- wrote myself
   meant to act like List.Extra.unique >> List.map
   This version doesn't require a hash function if nothing else
-}


uniqueMap : (a -> b) -> List a -> List b
uniqueMap f list =
    List.map f (uniqueMapHelper list [])


uniqueMapHelper : List a -> List a -> List a
uniqueMapHelper input seen =
    case input of
        [] ->
            []

        first :: rest ->
            if List.member first seen then
                uniqueMapHelper rest seen
            else
                first :: uniqueMapHelper rest (first :: seen)



--wrote myself since apparently a function that does the argument ignoring like
--this, isn't on package.elm-lang.org


ignoreFirstArg : (a -> b) -> (c -> a -> b)
ignoreFirstArg f =
    (\c a -> f a)



--Also wrote myself, didn't search extensively for it


filterOutListFromDict : List a -> Dict comparable a -> Dict comparable a
filterOutListFromDict list dict =
    Dict.filter
        (\key value ->
            List.member value list
                |> not
        )
        dict



-- derived from http://stackoverflow.com/a/33625733/4496839
-- has less helper functions than the List.Extra version


indexOfhelper : List a -> a -> Int -> Maybe Int
indexOfhelper lst elem offset =
    case lst of
        [] ->
            Nothing

        x :: xs ->
            if x == elem then
                Just offset
            else
                indexOfhelper xs elem (offset + 1)


indexOf : List a -> a -> Maybe Int
indexOf lst element =
    indexOfhelper lst element 0



-- A lot of the time this default is fine


indexOfDefault : List a -> a -> Int
indexOfDefault lst element =
    indexOfhelper lst element 0
        |> Maybe.withDefault -1



--wrote myself based on min-free example in https://github.com/liuxinyu95/AlgoXY,
-- I searched "elm binary search" but that gave me stuff about trees
--(probably overkill for this project)


getLowestAbsentInt : List Int -> Int
getLowestAbsentInt list =
    getLowestAbsentIntHelper list 0 (List.length list - 1)


getLowestAbsentIntHelper : List Int -> Int -> Int -> Int
getLowestAbsentIntHelper list lowerBound upperBound =
    if list == [] then
        lowerBound
    else
        let
            paritionPoint : Int
            paritionPoint =
                toFloat (lowerBound + upperBound)
                    / 2
                    |> floor

            ( lowerList, higherList ) =
                List.partition ((>=) paritionPoint) list
        in
            if List.length lowerList == (paritionPoint - lowerBound + 1) then
                getLowestAbsentIntHelper higherList (paritionPoint + 1) upperBound
            else
                getLowestAbsentIntHelper lowerList lowerBound paritionPoint



-- from https://github.com/krisajenkins/elm-exts


{-| Round a `Float` to a given number of decimal places.
-}
roundTo : Int -> Float -> Float
roundTo places value =
    let
        factor =
            10 ^ places
    in
        (value
            * factor
            |> round
            |> toFloat
        )
            / factor


mapFst f ( first, second ) =
    ( f first, second )


mapSnd f ( first, second ) =
    ( first, f second )
