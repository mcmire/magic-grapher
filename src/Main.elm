module Main exposing (main)

import Browser
import Browser.Events exposing (onKeyUp)
import Html exposing (Html, p, text)
import Json.Decode as D
import Svg exposing (svg)
import Svg.Attributes exposing (viewBox)


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type Model
    = WaitingForNodePlacement
    | Normal


init : () -> ( Model, Cmd Msg )
init _ =
    ( Normal, Cmd.none )



-- UPDATE


type Msg
    = WaitForPlacement
    | ReturnToNormal


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WaitForPlacement ->
            ( WaitingForNodePlacement, Cmd.none )

        ReturnToNormal ->
            ( Normal, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    onKeyUp (D.map mapKeyDecoder keyDecoder)


mapKeyDecoder : String -> Msg
mapKeyDecoder key =
    case key of
        "n" ->
            WaitingForNodePlacement


keyDecoder : D.Decoder String
keyDecoder =
    D.field "key" D.string



-- VIEW


view : Model -> Html Msg
view model =
    svg [ viewBox "0 0 1000 1100" ] []
