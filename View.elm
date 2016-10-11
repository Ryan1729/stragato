module View exposing (..)

import Model exposing (Model)
import Html exposing (Html, div, text)
import Msg exposing (Msg)
import DevControls


view : Model -> Html Msg
view =
    div []
        << DevControls.make
