module Model exposing (..)

import Mouse
import Math.Vector2 as V2 exposing (Vec2, vec2, getX, getY, add, scale)
import Points
import Array exposing (Array)
import Random exposing (Seed)


type alias Model =
    { pieceSelected : Maybe Int
    , piecePosition : Vec2
    , pieces : Array Piece
    , spaces : Spaces
    , gridWidth : Int
    , gridHeight : Int
    , spaceDeck : List SpaceType
    , seed : Seed
    , debug : Bool
    }


type alias Piece =
    { pieceType : PieceType
    , position : Vec2
    }


type PieceType
    = Star
    | WeirdThing


type alias Spaces =
    { positions : Array Vec2
    , types : Array SpaceType
    }


type SpaceType
    = Green
    | Red
    | EmptySpace


makeGridPoints width height =
    Points.hexGrid width height
        |> List.map (add (vec2 100 100) << V2.scale 60)
        |> Array.fromList


defaultWidth =
    4


defaultHeight =
    5


defaultSpaceDeck =
    [ Green, Green, Red, Red, EmptySpace ]


defaultState =
    { pieceSelected = Nothing
    , piecePosition = vec2 100 100
    , pieces = Array.fromList [ Piece Star (vec2 280 100), Piece WeirdThing (vec2 100 100) ]
    , spaces =
        fst
            <| makeSpaces defaultWidth
                defaultHeight
                defaultSpaceDeck
                (Random.initialSeed -42)
    , seed = (Random.initialSeed 42)
    , gridWidth = defaultWidth
    , gridHeight = defaultHeight
    , spaceDeck = defaultSpaceDeck
    , debug = True
    }


makeSpaces : Int -> Int -> List SpaceType -> Seed -> ( Spaces, Seed )
makeSpaces width height deck seed =
    let
        gridPoints =
            makeGridPoints width height

        ( spaceTypes, newSeed ) =
            fillArrayFromDeck EmptySpace deck (Array.length gridPoints) seed
    in
        ( Spaces gridPoints spaceTypes, newSeed )


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
