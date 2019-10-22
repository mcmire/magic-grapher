module Main exposing (main)

import Basics exposing (Int, floor)
import Browser
import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events exposing (onKeyUp, onResize)
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
import Svg.Events as SE
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
    | PositioningNodeAt Position


type State
    = CapturingMouseMovementsWhile MovingMouse
    | EditingNodeText Node
    | WaitingForFirstAction


type alias Node =
    { id : Int, x : Int, y : Int, text : String }


type alias Model =
    { state : State
    , lastNodeId : Int
    , nodes : List Node
    , nodeBeingPositioned : Maybe Node
    , viewboxWidth : Int
    , viewboxHeight : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { state = WaitingForFirstAction
      , lastNodeId = -1
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
    | PositionNodeAt Position
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

        PositionNodeAt pos ->
            ( { model | state = CapturingMouseMovementsWhile (PositioningNodeAt pos) }
            , Cmd.none
            )

        PlaceNodeAt pos ->
            let
                nextNodeId =
                    model.lastNodeId + 1

                newNode =
                    newNodeAt pos nextNodeId
            in
            ( { model
                | nodes = model.nodes ++ [ newNode ]
                , lastNodeId = nextNodeId
                , state = EditingNodeText newNode
              }
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


newNodeAt : Position -> Int -> Node
newNodeAt pos id =
    { id = id, x = pos.x, y = pos.y, text = "" }



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
            [ onKeyUp (D.map mapKeyDecoderWhenCapturingMouseMove keyDecoder) ]

        WaitingForFirstAction ->
            [ onKeyUp (D.map mapKeyDecoderForWaitingForFirstAction keyDecoder) ]

        _ ->
            []


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



-- VIEW


view : Model -> H.Html Msg
view model =
    H.div []
        [ H.div [ HA.class debug ] [ H.text ("State: " ++ modelName model) ]
        , S.svg
            (svgAttributes model)
            (nodeElements model ++ nodeElementToBePlaced model)
        ]


svgAttributes model =
    constantSvgAttributes model
        ++ svgAttributesWhileCapturingMouseMovements model


constantSvgAttributes model =
    [ SA.class world
    , SA.viewBox
        (joinIntsWith
            " "
            [ 0, 0, model.viewboxWidth, model.viewboxHeight ]
        )
    , SA.preserveAspectRatio "none"
    ]


svgAttributesWhileCapturingMouseMovements model =
    case model.state of
        CapturingMouseMovementsWhile _ ->
            [ SE.on "mousemove" (D.map PositionNodeAt mouseDecoder)
            , SE.on "click" (D.map PlaceNodeAt mouseDecoder)
            ]

        _ ->
            []


nodeElements : Model -> List (H.Html Msg)
nodeElements model =
    List.map nodeElement model.nodes


nodeElement : Node -> S.Svg Msg
nodeElement node =
    S.ellipse
        [ SA.cx (String.fromInt node.x)
        , SA.cy (String.fromInt node.y)
        , SA.rx "50"
        , SA.ry "30"
        , SA.fill "none"
        , SA.stroke (toCssString (hsla 0.6 0.7 0.75 1))
        , SA.strokeWidth "1px"
        ]
        []


nodeElementToBePlaced : Model -> List (H.Html Msg)
nodeElementToBePlaced model =
    case model.state of
        CapturingMouseMovementsWhile (PositioningNodeAt pos) ->
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
                    , SA.stroke (toCssString (hsla 0.6 0.7 0.75 0.6))
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

        CapturingMouseMovementsWhile (PositioningNodeAt pos) ->
            "Positioning node at: " ++ toString pos

        EditingNodeText node ->
            "Editing text for node " ++ String.fromInt node.id

        WaitingForFirstAction ->
            "Waiting for first action"



--- UTILITIES


joinIntsWith : String -> List Int -> String
joinIntsWith separator ints =
    String.join separator (List.map String.fromInt ints)
