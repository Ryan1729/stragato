module PlayfieldComponents exposing (..)

import Array exposing (Array)
import Array.Extra
import Math.Vector2 as V2 exposing (Vec2, vec2, add)
import Random exposing (Seed)
import Points
import Extras
import Dict exposing (Dict)


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


makeSpaces : Int -> Int -> List SpaceType -> Seed -> ( Spaces, Seed )
makeSpaces width height deck seed =
    let
        gridPoints =
            makeGridPoints width height

        ( spaceTypes, newSeed ) =
            fillListFromDeck EmptySpace deck (List.length gridPoints) seed

        spaces =
            List.map2 putSpaceTogether gridPoints spaceTypes
                |> Dict.fromList
    in
        ( spaces, newSeed )


putSpaceTogether : ( ( Int, Int ), Vec2 ) -> SpaceType -> ( ( Int, Int ), Space )
putSpaceTogether ( index, position ) spaceType =
    ( index, Space position spaceType )


makeGridPoints : Int -> Int -> List ( ( Int, Int ), Vec2 )
makeGridPoints width height =
    Points.hexGrid width height
        |> List.map
            (\( pair, vector ) ->
                ( pair
                , vector
                    |> V2.scale 60
                    |> add (vec2 100 100)
                )
            )


makePieces : Spaces -> List PieceType -> Seed -> ( Dict Int Piece, Seed )
makePieces spaces deck seed =
    let
        filteredPositions =
            filterPositions spaces

        ( pieceTypes, newSeed ) =
            fillListFromDeck NoPiece
                deck
                (List.length filteredPositions)
                seed

        pieces =
            List.map2 Piece pieceTypes filteredPositions
                |> List.indexedMap (,)
                |> Dict.fromList
    in
        ( pieces
        , newSeed
        )


filterPositions : Spaces -> List Vec2
filterPositions spaces =
    Dict.values spaces
        |> List.filterMap
            (\space ->
                if space.spaceType /= EmptySpace then
                    Just space.position
                else
                    Nothing
            )


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
