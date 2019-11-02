port module Main exposing (main)

import Basics exposing (floor)
import Browser
import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events exposing (onKeyUp, onResize)
import Color exposing (hsla)
import Color.Convert exposing (colorToCssHsla)
import Debouncer.Messages as Debouncer
    exposing
        ( Debouncer
        , provideInput
        , throttle
        , toDebouncer
        )
import Debug exposing (log, toString)
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as D
import Json.Encode as E
import List
import List.Extra
import Node exposing (Node)
import NodeCollection exposing (NodeCollection)
import RoundedRectangle exposing (roundedRect)
import String exposing (concat, slice)
import String.Extra exposing (insertAt)
import Styles.Main as Styles
import Svg as S
import Svg.Attributes as SA
import Svg.Events as SE
import Task
import Tuple
import Types exposing (Dimensions, Position, Range)


port calculateNodeContentMetrics : E.Value -> Cmd msg


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias NodeContentMetricsRecalculatedEvent =
    { nodeId : Node.Id
    , width : Float
    , height : Float
    , cursorPosition : Float
    , text : String
    }


type State
    = EditingNodeText Node.Id
    | WaitingForFirstAction
    | WaitingForNodeToBePlaced


type alias SvgTextElementAddedEvent =
    { nodeId : Node.Id
    , width : Float
    , height : Float
    }


type alias Model =
    { state : State
    , viewbox : Dimensions
    , mouse : { pos : Maybe Position, cursor : String }
    , debouncer : Debouncer Msg
    , nodes : NodeCollection
    , errorMessages : List String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { state = WaitingForFirstAction
      , viewbox = Dimensions 0 0
      , mouse = { pos = Nothing, cursor = "normal" }
      , debouncer = toDebouncer (throttle 250)
      , nodes = NodeCollection.empty
      , errorMessages = []
      }
    , Task.perform AdjustViewboxFromInitial getViewport
    )



-- UPDATE


type Direction
    = Left
    | Right



--| Up
--| Down


type NodeContentChange
    = InsertCharacter String
    | RemovePreviousCharacter
    | MoveCursor Direction


type Msg
    = AdjustViewboxFromInitial Viewport
    | AdjustViewboxFromResize Int Int
    | DebounceMsg (Debouncer.Msg Msg)
    | DisplayDecodeError D.Error
    | EditNodeText Node.Id
    | DoNothing
    | PlaceNodeAt Position
    | ReceiveRecalculatedNodeContentMetrics NodeContentMetricsRecalculatedEvent
    | ReturnToWaitingForFirstAction
    | UpdateNodeContent Node.Id NodeContentChange
    | TrackMouseMovementAt Position
    | WaitForNodeToBePlaced


updateDebouncer : Debouncer.UpdateConfig Msg Model
updateDebouncer =
    { mapMsg = DebounceMsg
    , getDebouncer = .debouncer
    , setDebouncer = \debouncer model -> { model | debouncer = debouncer }
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AdjustViewboxFromInitial viewport ->
            let
                { width, height } =
                    viewport.viewport
            in
            ( { model | viewbox = Dimensions width height }, Cmd.none )

        AdjustViewboxFromResize width height ->
            ( { model | viewbox = Dimensions (toFloat width) (toFloat height) }
            , Cmd.none
            )

        DebounceMsg subMsg ->
            Debouncer.update update updateDebouncer subMsg model

        DisplayDecodeError err ->
            ( { model | errorMessages = String.split "\n" (D.errorToString err) }
            , Cmd.none
            )

        DoNothing ->
            ( model, Cmd.none )

        EditNodeText nodeId ->
            let
                updatedModel =
                    { model
                        | nodes =
                            NodeCollection.updateNodeContentFor nodeId
                                (\content ->
                                    { content | isBeingEdited = True }
                                )
                                model.nodes
                    }
            in
            ( { updatedModel | state = EditingNodeText nodeId }, Cmd.none )

        UpdateNodeContent nodeId change ->
            let
                _ =
                    log "[Elm] updating node content" { nodeId = nodeId, change = change }

                maybeNode =
                    NodeCollection.get nodeId model.nodes
            in
            case maybeNode of
                Just node ->
                    let
                        cursorIndex =
                            case change of
                                InsertCharacter char ->
                                    node.content.cursorIndex + 1

                                RemovePreviousCharacter ->
                                    if node.content.cursorIndex > -1 then
                                        node.content.cursorIndex - 1

                                    else
                                        node.content.cursorIndex

                                MoveCursor dir ->
                                    case dir of
                                        Left ->
                                            if node.content.cursorIndex > -1 then
                                                node.content.cursorIndex - 1

                                            else
                                                node.content.cursorIndex

                                        Right ->
                                            if node.content.cursorIndex < (String.length node.content.text - 1) then
                                                node.content.cursorIndex + 1

                                            else
                                                node.content.cursorIndex

                        text =
                            case change of
                                InsertCharacter char ->
                                    insertAt char cursorIndex node.content.text

                                RemovePreviousCharacter ->
                                    slice 0 node.content.cursorIndex node.content.text
                                        ++ slice
                                            (node.content.cursorIndex + 1)
                                            (String.length node.content.text)
                                            node.content.text

                                _ ->
                                    node.content.text

                        newModel =
                            { model
                                | nodes =
                                    NodeCollection.updateNodeContentFor nodeId
                                        (\content ->
                                            { content
                                                | cursorIndex = cursorIndex
                                                , text = text
                                            }
                                        )
                                        model.nodes
                            }

                        cmd =
                            calculateNodeContentMetrics
                                (encodeCalculateNodeContentMetricsRequest
                                    node
                                    cursorIndex
                                    text
                                )
                    in
                    ( newModel, cmd )

                Nothing ->
                    ( { model
                        | errorMessages =
                            [ "Could not find node " ++ String.fromInt nodeId
                            ]
                      }
                    , Cmd.none
                    )

        PlaceNodeAt pos ->
            let
                ( newNodes, newNode ) =
                    NodeCollection.insert pos True model.nodes
            in
            -- TODO: Make a "select" msg?
            update (EditNodeText newNode.id) { model | nodes = newNodes }

        ReceiveRecalculatedNodeContentMetrics event ->
            let
                _ =
                    log "[Elm] receiving recalculated node content metrics" event
            in
            ( { model
                | nodes =
                    NodeCollection.updateNodeContentFor event.nodeId
                        (\content ->
                            { content
                                | width = event.width
                                , height = event.height
                                , cursorPosition = event.cursorPosition
                            }
                        )
                        model.nodes
              }
            , Cmd.none
            )

        ReturnToWaitingForFirstAction ->
            ( { model | state = WaitingForFirstAction }, Cmd.none )

        TrackMouseMovementAt pos ->
            let
                mouse =
                    model.mouse
            in
            ( { model | mouse = { mouse | pos = Just pos } }, Cmd.none )

        WaitForNodeToBePlaced ->
            ( { model | state = WaitingForNodeToBePlaced }
            , Cmd.none
            )


encodeCalculateNodeContentMetricsRequest : Node -> Int -> String -> E.Value
encodeCalculateNodeContentMetricsRequest node cursorIndex text =
    E.object
        [ ( "nodeId", Node.encodeId node.id )
        , ( "cursorIndex", E.int cursorIndex )
        , ( "text", E.string text )
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch (commonSubscriptions ++ specificSubscriptions model)


commonSubscriptions : List (Sub Msg)
commonSubscriptions =
    [ onResize AdjustViewboxFromResize ]


specificSubscriptions : Model -> List (Sub Msg)
specificSubscriptions model =
    case model.state of
        -- TODO: There's probably a way to match these up
        WaitingForFirstAction ->
            [ onKeyUp (D.map (mapKeyDecoder model) decodeKeyEvent) ]

        WaitingForNodeToBePlaced ->
            [ onKeyUp (D.map (mapKeyDecoder model) decodeKeyEvent) ]

        EditingNodeText _ ->
            [ onKeyUp (D.map (mapKeyDecoder model) decodeKeyEvent) ]


mapKeyDecoder : Model -> String -> Msg
mapKeyDecoder model key =
    case model.state of
        WaitingForFirstAction ->
            if key == "n" then
                WaitForNodeToBePlaced

            else
                DoNothing

        WaitingForNodeToBePlaced ->
            if key == "Escape" then
                ReturnToWaitingForFirstAction

            else
                DoNothing

        EditingNodeText nodeId ->
            -- TODO: Handle all kinds of keys and such
            if key == "Escape" then
                ReturnToWaitingForFirstAction

            else if String.length key == 1 then
                UpdateNodeContent nodeId (InsertCharacter key)

            else if key == "Backspace" then
                UpdateNodeContent nodeId RemovePreviousCharacter

            else if key == "ArrowLeft" then
                UpdateNodeContent nodeId (MoveCursor Left)

            else if key == "ArrowRight" then
                UpdateNodeContent nodeId (MoveCursor Right)
                --else if key == "ArrowUp" then
                --UpdateNodeContent node (MoveCursor Up)
                --else if key == "ArrowDown" then
                --UpdateNodeContent node (MoveCursor Down)

            else
                DoNothing


decodeKeyEvent : D.Decoder String
decodeKeyEvent =
    D.field "key" D.string


decodeMouseEvent : D.Decoder Position
decodeMouseEvent =
    D.map2 Position (D.field "clientX" D.float) (D.field "clientY" D.float)


decodeNodeContentMetricsRecalculatedEvent : D.Decoder NodeContentMetricsRecalculatedEvent
decodeNodeContentMetricsRecalculatedEvent =
    D.field "detail"
        (D.map5
            NodeContentMetricsRecalculatedEvent
            (D.field "nodeId" Node.decodeId)
            (D.field "width" D.float)
            (D.field "height" D.float)
            (D.field "cursorPosition" D.float)
            (D.field "text" D.string)
        )


decodeCharacterPosition : D.Decoder Range
decodeCharacterPosition =
    D.map2
        Range
        (D.field "start" D.int)
        (D.field "end" D.int)


view : Model -> H.Html Msg
view model =
    H.div
        [ HA.attribute "data-id" "root" ]
        (withErrorsContainer model (mainChildren model))


mainChildren : Model -> List (H.Html Msg)
mainChildren model =
    [ H.div
        [ HA.class Styles.debug ]
        [ H.text ("State: " ++ describeModelState model) ]
    , S.svg
        (svgAttributes model)
        (nodeElementToBePlaced model ++ placedNodeElements model)
    ]


withErrorsContainer : Model -> List (H.Html msg) -> List (H.Html msg)
withErrorsContainer model elements =
    if List.isEmpty model.errorMessages then
        elements

    else
        elements
            ++ [ H.div
                    [ HA.class Styles.errors ]
                    [ H.div
                        [ HA.class Styles.errorsHeader ]
                        [ H.text "Errors" ]
                    , H.div
                        [ HA.class Styles.errorsBody ]
                        (List.map
                            (\message -> H.p [] [ H.text message ])
                            model.errorMessages
                        )
                    ]
               ]


svgAttributes : Model -> List (S.Attribute Msg)
svgAttributes model =
    constantSvgAttributes model
        ++ svgAttributesWhileWaitingForNodeToBePlaced model


constantSvgAttributes : Model -> List (S.Attribute Msg)
constantSvgAttributes model =
    [ SA.class Styles.world
    , SA.viewBox
        (joinIntsWith
            " "
            [ 0, 0, floor model.viewbox.width, floor model.viewbox.height ]
        )
    , SA.preserveAspectRatio "none"
    , SE.on "mousemove"
        (D.map
            (debouncedVersionOf TrackMouseMovementAt)
            decodeMouseEvent
        )
    ]


debouncedVersionOf : (a -> Msg) -> (a -> Msg)
debouncedVersionOf wrapInMsg a =
    --DebounceMsg (provideInput (wrapInMsg a))
    wrapInMsg a


svgAttributesWhileWaitingForNodeToBePlaced : Model -> List (S.Attribute Msg)
svgAttributesWhileWaitingForNodeToBePlaced model =
    case model.state of
        WaitingForNodeToBePlaced ->
            [ SE.on "click" (D.map PlaceNodeAt decodeMouseEvent)
            ]

        _ ->
            []


nodeElementToBePlaced : Model -> List (H.Html Msg)
nodeElementToBePlaced model =
    case model.state of
        WaitingForNodeToBePlaced ->
            case model.mouse.pos of
                Just pos ->
                    [ nodeElement
                        (Node.unplacedAt pos)
                        [ SA.stroke (colorToCssHsla unselectedNodeBorderColor)
                        , SA.fill "white"
                        , SA.opacity "0.6"
                        ]
                    ]

                Nothing ->
                    []

        _ ->
            []


placedNodeElements : Model -> List (S.Svg Msg)
placedNodeElements model =
    List.map (placedNodeElement model) (NodeCollection.values model.nodes)


placedNodeElement : Model -> Node -> S.Svg Msg
placedNodeElement model node =
    let
        nodeBorderColor =
            if node.isSelected then
                selectedNodeBorderColor

            else
                unselectedNodeBorderColor
    in
    nodeElement
        node
        [ SA.stroke (colorToCssHsla nodeBorderColor)
        , SA.fill (colorToCssHsla unselectedNodeFillColor)
        ]


nodeElement : Node -> List (S.Attribute Msg) -> S.Svg Msg
nodeElement node attrs =
    S.g
        [ SA.transform
            ("translate("
                ++ String.fromFloat node.pos.x
                ++ " "
                ++ String.fromFloat node.pos.y
                ++ ")"
            )
        ]
        (nodeBackground node attrs
            ++ [ nodeForeground node attrs ]
            ++ [ nodeEditor node attrs ]
        )


nodeBackground : Node -> List (S.Attribute Msg) -> List (S.Svg Msg)
nodeBackground node attrs =
    if node.isSelected then
        let
            width =
                node.dims.width + 10

            height =
                node.dims.height + 10
        in
        [ S.rect
            (attrs
                ++ [ SA.x (String.fromFloat -(width / 2))
                   , SA.y (String.fromFloat -(height / 2))
                   , SA.width (String.fromFloat width)
                   , SA.height (String.fromFloat height)
                   , SA.fill (colorToCssHsla selectedNodeFillColor)
                   , SA.stroke "none"
                   ]
            )
            []
        ]

    else
        []


nodeForeground : Node -> List (S.Attribute Msg) -> S.Svg Msg
nodeForeground node attrs =
    let
        attrsWithPossibleStrokeWidth =
            if node.isSelected then
                attrs ++ [ SA.strokeWidth "2px" ]

            else
                attrs

        cursor =
            if node.content.isBeingEdited then
                "text"

            else
                "move"
    in
    roundedRect node.dims
        5
        (attrsWithPossibleStrokeWidth ++ [ SA.cursor cursor ])


nodeEditor : Node -> List (S.Attribute Msg) -> S.Svg Msg
nodeEditor node attrs =
    S.g
        [ HA.attribute "data-id" "node-editor"
        , HA.attribute "data-node-id" (String.fromInt node.id)
        ]
        ([ nodeText node [ HA.attribute "data-id" "text-visible" ]
         , nodeText node
            [ HA.attribute "data-id" "text-hidden"
            , HA.style "display" "none"
            ]
         ]
            ++ nodeEditorCursor node attrs
        )


nodeText : Node -> List (S.Attribute Msg) -> S.Svg Msg
nodeText node attrs =
    S.text_
        (attrs
            ++ [ SA.x (String.fromFloat 0)
               , SA.y (String.fromFloat 0)
               , SA.dominantBaseline "central"
               , SA.textAnchor "middle"
               , SA.fontSize (String.fromFloat node.content.fontSize ++ "px")
               , HA.attribute "data-width"
                    (String.fromFloat node.content.width)
               , HA.attribute "data-height"
                    (String.fromFloat node.content.height)
               , SE.on "metricsRecalculated"
                    -- This looks particularly heinous, but Elm "conveniently" fails
                    -- silently if it cannot decode the data within the event which is
                    -- emitted on the JavaScript side.
                    -- Source for this solution: <https://github.com/Janiczek/elm-docs/issues/40>
                    (D.value
                        |> D.andThen
                            (\value ->
                                case D.decodeValue decodeNodeContentMetricsRecalculatedEvent value of
                                    Ok event ->
                                        D.succeed (ReceiveRecalculatedNodeContentMetrics event)

                                    Err err ->
                                        D.succeed (DisplayDecodeError err)
                            )
                    )
               ]
        )
        [ S.text node.content.text ]


nodeEditorCursor : Node -> List (S.Attribute msg) -> List (S.Svg msg)
nodeEditorCursor node attrs =
    if node.content.isBeingEdited then
        let
            nodePadding =
                10.0
        in
        [ S.rect
            [ SA.x (String.fromFloat node.content.cursorPosition)
            , SA.y (String.fromFloat (-node.content.fontSize / 2))
            , SA.width "2"
            , SA.height (String.fromFloat node.content.fontSize)
            , SA.shapeRendering "crispEdges"
            , SA.class Styles.blink
            ]
            []
        ]

    else
        []


describeModelState : Model -> String
describeModelState model =
    case model.state of
        EditingNodeText nodeId ->
            "Editing text for node " ++ String.fromInt nodeId

        WaitingForFirstAction ->
            "Waiting for first action"

        WaitingForNodeToBePlaced ->
            "Waiting for node to be placed"



--- UTILITIES


unselectedNodeBorderColor =
    hsla (degrees 228) 0.57 0.68 1


selectedNodeBorderColor =
    hsla (degrees 228) 0.85 0.56 1


unselectedNodeFillColor =
    hsla (degrees 228) 0.56 0.91 1


selectedNodeFillColor =
    hsla (degrees 228) 0.56 0.91 0.6


joinIntsWith : String -> List Int -> String
joinIntsWith separator ints =
    String.join separator (List.map String.fromInt ints)
