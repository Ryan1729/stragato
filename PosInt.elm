module PosInt exposing (PosInt, fromInt, toInt)

{-|
Because the constructor for `PosInt` is not exported, you can be
confident that if you are using a `PosInt`, then it's a positive integer.
@docs PosInt, fromInt, toInt
-}

--inspired by https://github.com/Fresheyeball/elm-restrict-number
--specifically Natural.elm, but I wanted to actually get the
--value outside of a maybe without defaulting to Debug.crash or whatever


{-| -}
type PosInt
    = PosInt Int


{-| -}
fromInt : Int -> PosInt
fromInt x =
    if x > 0 then
        PosInt x
    else
        PosInt 1


{-| -}
toInt : PosInt -> Int
toInt (PosInt x) =
    x
