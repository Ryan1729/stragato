module Extras exposing (..)

import Array exposing (Array)


{- from elm-community/array-extra -}


{-| Update the element at the index using a function. Returns the array unchanged if the index is out of bounds.
    update  1 ((+)10) (fromList [1,2,3]) == fromList [1,12,3]
    update  4 ((+)10) (fromList [1,2,3]) == fromList [1,2,3]
    update -1 ((+)10) (fromList [1,2,3]) == fromList [1,2,3]
-}
update : Int -> (a -> a) -> Array a -> Array a
update n f a =
    let
        element =
            Array.get n a
    in
        case element of
            Nothing ->
                a

            Just element' ->
                Array.set n (f element') a