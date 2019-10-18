module Main exposing (main)

import Browser
import Css.Global exposing (body, global)
import Html exposing (Html, p, text)
import Svg exposing (..)
import Svg.Attributes exposing (..)


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


type alias Model =
    { mode : String }


init : Model
init =
    { mode = "normal" }


type Msg
    = WaitForPlacement
    | ReturnToNormal


update : Msg -> Model -> Model
update msg model =
    case msg of
        WaitForPlacement ->
            { model | mode = "waitForPlacement" }

        ReturnToNormal ->
            { model | mode = "normal" }


view : Model -> Html Msg
view model =
    svg [ viewBox "0 0 1000 1100" ] []
