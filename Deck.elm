module Deck exposing (..)

import Random exposing (Seed)


fillListFromDeck : a -> List a -> Int -> Seed -> ( List a, Seed )
fillListFromDeck default deck length seed =
    fillListFromDeckHelper default deck deck length seed []


fillListFromDeckHelper : a -> List a -> List a -> Int -> Seed -> List a -> ( List a, Seed )
fillListFromDeckHelper default deck currentDeck remainingLength seed result =
    if remainingLength == 0 then
        ( result, seed )
    else
        let
            ( drawnElement, newDeck, newSeed ) =
                drawFromDeck default deck currentDeck seed
        in
            fillListFromDeckHelper default
                deck
                newDeck
                (remainingLength - 1)
                newSeed
                (drawnElement :: result)


drawFromDeck : a -> List a -> List a -> Seed -> ( a, List a, Seed )
drawFromDeck default deck currentDeck seed =
    case maybeFromDeck currentDeck seed of
        Nothing ->
            case maybeFromDeck deck seed of
                Nothing ->
                    ( default, deck, seed )

                Just result ->
                    result

        Just result ->
            result


maybeFromDeck : List a -> Seed -> Maybe ( a, List a, Seed )
maybeFromDeck currentDeck seed =
    let
        ( index, newSeed ) =
            Random.step (Random.int 0 <| List.length currentDeck - 1) seed
    in
        case List.head <| List.drop index currentDeck of
            Nothing ->
                Nothing

            Just drawnElement ->
                let
                    remainingElements =
                        List.take index currentDeck
                            ++ List.drop (index + 1) currentDeck
                in
                    Just ( drawnElement, remainingElements, newSeed )
