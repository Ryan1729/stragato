module QuantityControl exposing (..)

import Html exposing (Html, Attribute, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onInput, onClick)
import DevControlsCommon as DCC
import String


type alias QuantityControl a msg =
    List a -> a -> Html msg


standard : (a -> Int -> msg) -> (a -> Int -> msg) -> (a -> Int -> msg) -> QuantityControl a msg
standard removeMessage addMessage noChangeMessage dataList item =
    let
        currentAmount =
            amountOfItemInDeck item dataList
    in
        Html.input
            [ Html.Attributes.type' "number"
            , Html.Attributes.min "0"
            , Html.Attributes.step "any"
            , String.toInt
                >> Result.withDefault currentAmount
                >> (\newAmount ->
                        if newAmount > currentAmount then
                            addMessage item (newAmount - currentAmount)
                        else if newAmount >= 0 && newAmount < currentAmount then
                            removeMessage item (currentAmount - newAmount)
                        else
                            noChangeMessage item currentAmount
                   )
                |> onInput
            , currentAmount |> toString |> Html.Attributes.value
            , style [ ( "width", "4rem" ), ( "background-color", DCC.background ) ]
            ]
            []


confirmRemoval : (a -> Int -> msg) -> (a -> Int -> msg) -> (a -> Int -> msg) -> QuantityControl a msg
confirmRemoval removeMessage addMessage noChangeMessage dataList item =
    let
        currentAmount =
            amountOfItemInDeck item dataList

        input =
            Html.input
                [ Html.Attributes.type' "number"
                , Html.Attributes.min "1"
                , Html.Attributes.step "any"
                , String.toInt
                    >> Result.withDefault currentAmount
                    >> (\newAmount ->
                            if newAmount > currentAmount then
                                addMessage item (newAmount - currentAmount)
                            else if newAmount >= 1 && newAmount < currentAmount then
                                removeMessage item (currentAmount - newAmount)
                            else
                                noChangeMessage item currentAmount
                       )
                    |> onInput
                , currentAmount |> toString |> Html.Attributes.value
                , style [ ( "width", "4rem" ), ( "background-color", DCC.background ) ]
                ]
                []
    in
        if currentAmount <= 1 then
            div []
                [ input
                , Html.button
                    [ onClick (removeMessage item 1)
                    , style [ ( "border", "none" ), ( "background", "inherit" ), ( "color", "red" ) ]
                    ]
                    [ text "×" ]
                ]
        else
            input


amountOfItemInDeck : a -> List a -> Int
amountOfItemInDeck item deck =
    List.filter ((==) item) deck
        |> List.length
