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
import Dict exposing (Dict)
import Html as H
import Html.Attributes as HA
import Json.Decode as D
import Json.Encode as E
import List
import List.Extra
import Node exposing (Node)
import NodeCollection exposing (NodeCollection)
import RoundedRectangle exposing (roundedRect)
import String exposing (concat)
import Styles.Main as Styles
import Svg as S
import Svg.Attributes as SA
import Svg.Events as SE
import Task
import Tuple
import Types exposing (Dimensions, Position)


port onSvgTextElementAdded : (D.Value -> msg) -> Sub msg


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


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


type Input
    = Character String
    | Backspace


type Msg
    = AdjustViewboxFromInitial Viewport
    | AdjustViewboxFromResize Int Int
    | CaptureNodeTextBoundingBox (Result D.Error SvgTextElementAddedEvent)
    | DebounceMsg (Debouncer.Msg Msg)
    | EditNodeText Node.Id
    | InputNodeContent Node.Id Input
    | DoNothing
    | PlaceNodeAt Position
    | ReturnToWaitingForFirstAction
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

        -- Pulled from many sources
        CaptureNodeTextBoundingBox result ->
            let
                newModel =
                    case result of
                        Ok event ->
                            { model
                                | nodes =
                                    NodeCollection.updateNodeContentFor event.nodeId
                                        (\content ->
                                            { content
                                                | width = event.width
                                                , height = event.height
                                            }
                                        )
                                        model.nodes
                            }

                        Err error ->
                            { model
                                | errorMessages =
                                    String.split
                                        "\n"
                                        (D.errorToString error)
                            }
            in
            ( newModel, Cmd.none )

        DebounceMsg subMsg ->
            Debouncer.update update updateDebouncer subMsg model

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

        InputNodeContent nodeId input ->
            let
                updatedNodes =
                    NodeCollection.updateNodeContentFor
                        nodeId
                        (\content ->
                            case input of
                                Character char ->
                                    { content | text = content.text ++ char }

                                Backspace ->
                                    { content | text = String.dropRight 1 content.text }
                        )
                        model.nodes
            in
            ( { model | nodes = updatedNodes }, Cmd.none )

        PlaceNodeAt pos ->
            let
                ( newNodes, newNode ) =
                    NodeCollection.insert pos True model.nodes
            in
            -- TODO: Make a "select" msg?
            update (EditNodeText newNode.id) { model | nodes = newNodes }

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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch (commonSubscriptions ++ specificSubscriptions model)


commonSubscriptions : List (Sub Msg)
commonSubscriptions =
    [ onResize AdjustViewboxFromResize
    , onSvgTextElementAdded
        (decodeSvgTextElementAddedEvent >> CaptureNodeTextBoundingBox)
    ]


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
                InputNodeContent nodeId (Character key)

            else if key == "Backspace" then
                InputNodeContent nodeId Backspace

            else
                DoNothing


decodeKeyEvent : D.Decoder String
decodeKeyEvent =
    D.field "key" D.string


decodeMouseEvent : D.Decoder Position
decodeMouseEvent =
    D.map2 Position (D.field "clientX" D.float) (D.field "clientY" D.float)


decodeSvgTextElementAddedEvent : D.Value -> Result D.Error SvgTextElementAddedEvent
decodeSvgTextElementAddedEvent =
    D.decodeValue
        (D.map3
            SvgTextElementAddedEvent
            (D.field "nodeId" Node.decodeId)
            (D.field "width" D.float)
            (D.field "height" D.float)
        )


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


withErrorsContainer : Model -> List (H.Html Msg) -> List (H.Html Msg)
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


placedNodeElements : Model -> List (S.Svg msg)
placedNodeElements model =
    List.map (placedNodeElement model) (NodeCollection.values model.nodes)


placedNodeElement : Model -> Node -> S.Svg msg
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


nodeElement : Node -> List (S.Attribute msg) -> S.Svg msg
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
            ++ [ nodeText node ]
            ++ nodeEditor node attrs
        )


nodeForeground : Node -> List (S.Attribute msg) -> S.Svg msg
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


nodeBackground : Node -> List (S.Attribute msg) -> List (S.Svg msg)
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


nodeEditor : Node -> List (S.Attribute msg) -> List (S.Svg msg)
nodeEditor node attrs =
    if node.content.isBeingEdited then
        let
            nodePadding =
                10.0
        in
        [ S.rect
            [ SA.x (String.fromFloat (node.content.width / 2))
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


nodeText : Node -> S.Svg msg
nodeText node =
    S.text_
        [ SA.x (String.fromFloat 0)
        , SA.y (String.fromFloat 0)
        , SA.dominantBaseline "central"
        , SA.textAnchor "middle"
        , SA.fontSize (String.fromFloat node.content.fontSize ++ "px")
        , SA.class Styles.text
        , HA.attribute "data-node-id" (String.fromInt node.id)
        , HA.attribute "data-width"
            (String.fromFloat node.content.width)
        , HA.attribute "data-height"
            (String.fromFloat node.content.height)
        ]
        [ S.text node.content.text ]


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
