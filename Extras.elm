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
