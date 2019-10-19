module Main exposing (main)

import Basics exposing (Int, floor)
import Browser
import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events exposing (onKeyUp, onMouseMove, onResize)
import Color exposing (hsla, toCssString)
import Debug exposing (log, toString)
import Html as H
import Html.Attributes as HA
import Json.Decode as D
import List
import String exposing (concat)
import Styles.Main exposing (debug, world)
import Svg as S
import Svg.Attributes as SA
import Task


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type MovingMouse
    = WaitingForNodePlacement
    | PlacingNodeAt Position


type State
    = CapturingMouseMovementsWhile MovingMouse
    | WaitingForFirstAction


type alias Node =
    { x : Int, y : Int, text : String }


type alias Model =
    { state : State
    , nodes : List Node
    , nodeBeingPositioned : Maybe Node
    , viewboxWidth : Int
    , viewboxHeight : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { state = WaitingForFirstAction
      , nodes = []
      , nodeBeingPositioned = Nothing
      , viewboxWidth = 0
      , viewboxHeight = 0
      }
    , Task.perform AdjustViewboxFromInitial getViewport
    )



-- UPDATE


type Msg
    = WaitForPlacement
    | ReturnToWaitingForFirstAction
    | PlaceNodeAt Position
    | AdjustViewboxFromInitial Viewport
    | AdjustViewboxFromResize Int Int
    | DoNothing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WaitForPlacement ->
            ( { model | state = CapturingMouseMovementsWhile WaitingForNodePlacement }
            , Cmd.none
            )

        ReturnToWaitingForFirstAction ->
            ( { model | state = WaitingForFirstAction }, Cmd.none )

        PlaceNodeAt pos ->
            ( { model | state = CapturingMouseMovementsWhile (PlacingNodeAt pos) }
            , Cmd.none
            )

        AdjustViewboxFromInitial viewport ->
            ( { model
                | viewboxWidth = floor viewport.viewport.width
                , viewboxHeight = floor viewport.viewport.height
              }
            , Cmd.none
            )

        AdjustViewboxFromResize width height ->
            ( { model | viewboxWidth = width, viewboxHeight = height }, Cmd.none )

        DoNothing ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


type alias Position =
    { x : Int, y : Int }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch (commonSubscriptions ++ specificSubscriptions model)


commonSubscriptions : List (Sub Msg)
commonSubscriptions =
    [ onResize AdjustViewboxFromResize ]


specificSubscriptions : Model -> List (Sub Msg)
specificSubscriptions model =
    case model.state of
        CapturingMouseMovementsWhile _ ->
            [ onMouseMove (D.map mapMouseDecoder mouseDecoder)
            , onKeyUp (D.map mapKeyDecoderWhenCapturingMouseMove keyDecoder)
            ]

        WaitingForFirstAction ->
            [ onKeyUp (D.map mapKeyDecoderForWaitingForFirstAction keyDecoder) ]


mapKeyDecoderForWaitingForFirstAction : String -> Msg
mapKeyDecoderForWaitingForFirstAction key =
    let
        _ =
            log "(waiting for first action) Key: " key
    in
    if key == "n" then
        WaitForPlacement

    else
        DoNothing


mapKeyDecoderWhenCapturingMouseMove : String -> Msg
mapKeyDecoderWhenCapturingMouseMove key =
    let
        _ =
            log "(capturing mouse move) Key: " key
    in
    if key == "Escape" then
        ReturnToWaitingForFirstAction

    else
        DoNothing


keyDecoder : D.Decoder String
keyDecoder =
    D.field "key" D.string


mouseDecoder : D.Decoder Position
mouseDecoder =
    D.map2 Position (D.field "clientX" D.int) (D.field "clientY" D.int)


mapMouseDecoder : Position -> Msg
mapMouseDecoder pos =
    PlaceNodeAt pos



-- VIEW


view : Model -> H.Html Msg
view model =
    H.div []
        [ H.div [ HA.class debug ] [ H.text ("State: " ++ modelName model) ]
        , S.svg
            [ SA.class world
            , SA.viewBox
                (joinIntsWith
                    " "
                    [ 0, 0, model.viewboxWidth, model.viewboxHeight ]
                )
            , SA.preserveAspectRatio "none"
            ]
            (groupForNodeToBePlaced model)
        ]


groupForNodeToBePlaced : Model -> List (H.Html Msg)
groupForNodeToBePlaced model =
    case model.state of
        CapturingMouseMovementsWhile (PlacingNodeAt pos) ->
            [ S.g
                [ SA.transform
                    (concat
                        [ "translate("
                        , String.fromInt pos.x
                        , " "
                        , String.fromInt pos.y
                        , ")"
                        ]
                    )
                ]
                [ S.ellipse
                    [ SA.cx "0"
                    , SA.cy "0"
                    , SA.rx "50"
                    , SA.ry "30"
                    , SA.fill "none"

                    --, SA.stroke (toCssString (hsla 0.6 0.7 0.75 0.4))
                    , SA.stroke "black"
                    , SA.strokeWidth "1px"
                    ]
                    []
                ]
            ]

        _ ->
            []


modelName : Model -> String
modelName model =
    case model.state of
        CapturingMouseMovementsWhile WaitingForNodePlacement ->
            "Waiting for node placement"

        CapturingMouseMovementsWhile (PlacingNodeAt pos) ->
            "Placing node at: " ++ toString pos

        WaitingForFirstAction ->
            "Waiting for first action"



--- UTILITIES


joinIntsWith : String -> List Int -> String
joinIntsWith separator ints =
    String.join separator (List.map String.fromInt ints)
