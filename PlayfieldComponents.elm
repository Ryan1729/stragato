module PlayfieldComponents exposing (..)

import Array exposing (Array)
import Array.Extra
import Math.Vector2 as V2 exposing (Vec2, vec2, add)
import Random exposing (Seed)
import Points
import Extras


makeGridPoints width height =
    Points.hexGrid width height
        |> List.map (add (vec2 100 100) << V2.scale 60)
        |> Array.fromList


type alias Piece =
    { pieceType : PieceType
    , position : Vec2
    }


type PieceControllability
    = Player
    | Computer
    | Both
    | None


pieceControllabilityPossibilities =
    [ Player
    , Computer
    , Both
    , None
    ]


type PieceType
    = Star PieceControllability
    | WeirdThing PieceControllability
    | Triangle PieceControllability
    | NoPiece


controllablePossibilities =
    List.concatMap
        (\f ->
            List.concatMap
                (\x ->
                    [ f x ]
                )
                pieceControllabilityPossibilities
        )
        [ Star, WeirdThing, Triangle ]


pieceTypePossibilities =
    controllablePossibilities ++ [ NoPiece ]


isActualPiece : Piece -> Bool
isActualPiece piece =
    piece.pieceType /= NoPiece


type alias Spaces =
    { positions : Array Vec2
    , types : Array SpaceType
    }


type SpaceType
    = Green
    | Red
    | Yellow
    | EmptySpace


spaceTypePossibilities =
    [ Green, Red, Yellow, EmptySpace ]


makeSpaces : Int -> Int -> List SpaceType -> Seed -> ( Spaces, Seed )
makeSpaces width height deck seed =
    let
        gridPoints =
            makeGridPoints width height

        ( spaceTypes, newSeed ) =
            fillArrayFromDeck EmptySpace deck (Array.length gridPoints) seed
    in
        ( Spaces gridPoints spaceTypes, newSeed )


makePieces : Spaces -> List PieceType -> Seed -> ( Array Piece, Seed )
makePieces { positions, types } deck seed =
    let
        filteredPositions =
            filterPositions positions types

        ( pieceTypes, newSeed ) =
            fillArrayFromDeck NoPiece
                deck
                (Array.length filteredPositions)
                seed
    in
        ( Array.Extra.map2 Piece pieceTypes filteredPositions
        , newSeed
        )


filterPositions positions types =
    let
        maxIndex =
            min (Array.length positions) (Array.length types) - 1
    in
        [0..maxIndex]
            |> List.filterMap
                (getFromArrayPredicatedOnOtherArray ((/=) EmptySpace)
                    positions
                    types
                )
            |> Array.fromList


getFromArrayPredicatedOnOtherArray : (b -> Bool) -> Array a -> Array b -> Int -> Maybe a
getFromArrayPredicatedOnOtherArray predicate array predicatedArray index =
    Array.get index predicatedArray
        |> Maybe.map predicate
        |> (flip Maybe.andThen)
            (\passes ->
                if passes then
                    Array.get index array
                else
                    Nothing
            )


fillArrayFromDeck : a -> List a -> Int -> Seed -> ( Array a, Seed )
fillArrayFromDeck default deck length seed =
    fillArrayFromDeckHelper default deck deck length seed Array.empty


fillArrayFromDeckHelper : a -> List a -> List a -> Int -> Seed -> Array a -> ( Array a, Seed )
fillArrayFromDeckHelper default deck currentDeck remainingLength seed result =
    if remainingLength == 0 then
        ( result, seed )
    else
        let
            ( drawnElement, newDeck, newSeed ) =
                drawFromDeck default deck currentDeck seed
        in
            fillArrayFromDeckHelper default
                deck
                newDeck
                (remainingLength - 1)
                newSeed
                (Array.push drawnElement result)


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
