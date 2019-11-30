module Main exposing (main)

import Browser
import Browser.Dom as Dom exposing (Viewport)
import Browser.Events as BE
import Color exposing (hsla)
import Color.Convert exposing (colorToCssHsla)
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as D
import Json.Encode as E
import Keyboard.Event exposing (KeyboardEvent, decodeKeyboardEvent)
import Keyboard.Key as Key
import List
import List.Extra
import Node exposing (Node)
import NodeCollection exposing (NodeCollection)
import NodeContent
import NodeId exposing (NodeId)
import RoundedRectangle exposing (roundedRect)
import String exposing (concat, slice)
import Svg as S
import Svg.Attributes as SA
import Svg.Events as SE
import Svg.Keyed as SK
import Task
import Tuple
import Types exposing (Dimensions, Position, Range)
import VirtualDom as V


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type NodeType
    = Placed
    | Unplaced


type State
    = UpdatingNodeContent NodeId
    | WaitingForFirstAction
    | WaitingForNodeToBePlaced


type alias SvgTextElementAddedEvent =
    { nodeId : NodeId
    , width : Float
    , height : Float
    }


type alias Model =
    { state : State
    , viewbox : Dimensions
    , mouse : { pos : Maybe Position, cursor : String }
    , nodes : NodeCollection
    , nodeContainingMouseDown : Maybe NodeId
    , errorMessages : List String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { state = WaitingForFirstAction
      , viewbox = Dimensions 0 0
      , mouse = { pos = Nothing, cursor = "normal" }
      , nodes = NodeCollection.empty
      , nodeContainingMouseDown = Nothing
      , errorMessages = []
      }
    , Task.perform AdjustViewboxFromInitial Dom.getViewport
    )



-- UPDATE


type Msg
    = AdjustViewboxFromInitial Viewport
    | AdjustViewboxFromResize Int Int
    | DisplayCouldNotFindNodeError NodeId
    | DisplayDecodeError D.Error
    | DoNothing
    | MouseDownInNodeContent NodeId Position
    | MouseUp
    | PlaceNodeAt Position
    | ReturnToWaitingForFirstAction
    | UpdateNodeContent NodeId NodeContent.Msg
    | TrackMouseMovementAt Position
    | WaitForNodeToBePlaced


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

        DisplayCouldNotFindNodeError nodeId ->
            ( { model
                | errorMessages =
                    [ "Could not find node by id: " ++ String.fromInt nodeId
                    ]
              }
            , Cmd.none
            )

        DisplayDecodeError err ->
            ( { model | errorMessages = String.split "\n" (D.errorToString err) }
            , Cmd.none
            )

        DoNothing ->
            ( model, Cmd.none )

        MouseDownInNodeContent nodeId pos ->
            update
                (UpdateNodeContent nodeId (NodeContent.updateCursorPositionFromMouse pos))
                { model | nodeContainingMouseDown = Just nodeId }

        MouseUp ->
            let
                _ =
                    Debug.log "Mouse UP!"
            in
            -- TODO: End the selection
            ( { model | nodeContainingMouseDown = Nothing }, Cmd.none )

        PlaceNodeAt pos ->
            let
                ( newNodes, newNode ) =
                    NodeCollection.insert pos True model.nodes
            in
            ( { model | nodes = newNodes }, Cmd.none )

        ReturnToWaitingForFirstAction ->
            ( { model | state = WaitingForFirstAction }, Cmd.none )

        TrackMouseMovementAt pos ->
            let
                mouse =
                    model.mouse
            in
            ( { model | mouse = { mouse | pos = Just pos } }, Cmd.none )

        UpdateNodeContent nodeId subMsg ->
            let
                result =
                    NodeContent.mapToDisplayDecodeError subMsg
            in
            case result of
                Just error ->
                    update (DisplayDecodeError error) model

                _ ->
                    let
                        maybeNode =
                            NodeCollection.get nodeId model.nodes
                    in
                    case maybeNode of
                        Just node ->
                            let
                                ( newContent, cmd ) =
                                    NodeContent.update
                                        subMsg
                                        node.content

                                newState =
                                    UpdatingNodeContent nodeId

                                newModel =
                                    { model
                                        | state = newState
                                        , nodes =
                                            NodeCollection.updateNodeContentFor nodeId
                                                (\content -> newContent)
                                                model.nodes
                                    }
                            in
                            ( newModel, Cmd.map (UpdateNodeContent nodeId) cmd )

                        Nothing ->
                            update (DisplayCouldNotFindNodeError nodeId) model

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
    [ BE.onResize AdjustViewboxFromResize ]


specificSubscriptions : Model -> List (Sub Msg)
specificSubscriptions model =
    let
        decoder =
            D.map (mapKeyboardEventToMsg model) decodeKeyboardEvent

        upSubs =
            [ BE.onKeyUp decoder ]
    in
    case model.state of
        WaitingForFirstAction ->
            upSubs

        WaitingForNodeToBePlaced ->
            upSubs

        UpdatingNodeContent nodeId ->
            let
                maybeNode =
                    NodeCollection.get nodeId model.nodes
            in
            case maybeNode of
                Just node ->
                    [ Sub.map
                        (UpdateNodeContent nodeId)
                        (NodeContent.subscriptions node.content)
                    ]

                Nothing ->
                    []


mapKeyboardEventToMsg : Model -> KeyboardEvent -> Msg
mapKeyboardEventToMsg model keyboardEvent =
    case model.state of
        WaitingForFirstAction ->
            if keyboardEvent.keyCode == Key.N then
                WaitForNodeToBePlaced

            else
                DoNothing

        WaitingForNodeToBePlaced ->
            if keyboardEvent.keyCode == Key.Escape then
                ReturnToWaitingForFirstAction

            else
                DoNothing

        _ ->
            DoNothing


mapMsg : Maybe Msg -> Msg
mapMsg maybeMsg =
    case maybeMsg of
        Just msg ->
            msg

        Nothing ->
            DoNothing


decodeMouseEvent : D.Decoder Position
decodeMouseEvent =
    D.map2 Position (D.field "clientX" D.float) (D.field "clientY" D.float)



-- VIEW


view : Model -> H.Html Msg
view model =
    H.div
        [ HA.attribute "data-id" "root" ]
        (mainChildren model)


mainChildren : Model -> List (H.Html Msg)
mainChildren model =
    [ H.div
        [ HA.class "info-overlay" ]
        (withErrorsContainer model
            [ H.div
                [ HA.class "debug" ]
                [ H.text ("State: " ++ describeModelState model) ]
            ]
        )
    , SK.node "svg"
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
                    [ HA.class "errors" ]
                    [ H.div
                        [ HA.class "errors__header" ]
                        [ H.text "Errors" ]
                    , H.div
                        [ HA.class "errors__body" ]
                        [ H.pre []
                            [ H.text (String.join "\n" model.errorMessages) ]
                        ]
                    ]
               ]


svgAttributes : Model -> List (S.Attribute Msg)
svgAttributes model =
    constantSvgAttributes model
        ++ svgAttributesWhileNoNodeIsBeingEdited model
        ++ svgAttributesWhileWaitingForNodeToBePlaced model
        ++ svgAttributesWhileMouseIsDownWithinNodeContent model


constantSvgAttributes : Model -> List (S.Attribute Msg)
constantSvgAttributes model =
    [ SA.class "world"
    , SA.viewBox
        (joinIntsWith
            " "
            [ 0, 0, floor model.viewbox.width, floor model.viewbox.height ]
        )
    , SA.preserveAspectRatio "none"
    ]


svgAttributesWhileNoNodeIsBeingEdited : Model -> List (S.Attribute Msg)
svgAttributesWhileNoNodeIsBeingEdited model =
    if NodeCollection.anyBeingEdited model.nodes then
        []

    else
        [ SE.on "mousemove" (D.map TrackMouseMovementAt decodeMouseEvent) ]


svgAttributesWhileWaitingForNodeToBePlaced : Model -> List (S.Attribute Msg)
svgAttributesWhileWaitingForNodeToBePlaced model =
    case model.state of
        WaitingForNodeToBePlaced ->
            [ SE.on "mousedown" (D.map PlaceNodeAt decodeMouseEvent) ]

        _ ->
            []


svgAttributesWhileMouseIsDownWithinNodeContent : Model -> List (S.Attribute Msg)
svgAttributesWhileMouseIsDownWithinNodeContent model =
    case model.nodeContainingMouseDown of
        Just _ ->
            [ SE.on "mouseup" (D.succeed MouseUp) ]

        Nothing ->
            []


nodeElementToBePlaced : Model -> List ( String, S.Svg Msg )
nodeElementToBePlaced model =
    case model.state of
        WaitingForNodeToBePlaced ->
            case model.mouse.pos of
                Just pos ->
                    [ ( "unplacedNode", unplacedNodeElement model pos ) ]

                Nothing ->
                    []

        _ ->
            []


placedNodeElements : Model -> List ( String, S.Svg Msg )
placedNodeElements model =
    List.map (placedNodeElement model) (NodeCollection.values model.nodes)


placedNodeElement : Model -> Node -> ( String, S.Svg Msg )
placedNodeElement model node =
    let
        nodeBorderColor =
            if node.isSelected then
                selectedNodeBorderColor

            else
                unselectedNodeBorderColor
    in
    ( "placedNode" ++ String.fromInt node.id
    , nodeElement
        model
        node
        [ HA.attribute "data-id" "graph-node"
        , HA.attribute "data-graph-node-id" (String.fromInt node.id)
        ]
        [ SA.stroke (colorToCssHsla nodeBorderColor)
        , SA.fill (colorToCssHsla unselectedNodeFillColor)
        ]
    )


unplacedNodeElement : Model -> Position -> S.Svg Msg
unplacedNodeElement model pos =
    nodeElement
        model
        (Node.unplacedAt pos)
        []
        [ SA.stroke (colorToCssHsla unselectedNodeBorderColor)
        , SA.fill "white"
        , SA.opacity "0.6"
        ]


nodeElement :
    Model
    -> Node
    -> List (S.Attribute Msg)
    -> List (S.Attribute Msg)
    -> S.Svg Msg
nodeElement model node groupAttrs nodeAttrs =
    let
        withPossibleOnMouseDown attrs =
            if node.content.isBeingEdited then
                attrs
                    ++ [ SE.preventDefaultOn "mousedown"
                            (D.map
                                (\msg -> ( msg, True ))
                                (D.map (MouseDownInNodeContent node.id) decodeMouseEvent)
                            )
                       ]

            else
                attrs

        isMouseDownInContentFor nodeId =
            model.nodeContainingMouseDown == Just nodeId

        withPossibleOnMouseMove attrs =
            if isMouseDownInContentFor node.id then
                case node.content.userLocation of
                    Just userLocation ->
                        attrs
                            ++ [ SE.on "mousemove"
                                    (D.map
                                        (UpdateNodeContent node.id)
                                        (D.map
                                            NodeContent.updateSelectionFromMouse
                                            decodeMouseEvent
                                        )
                                    )
                               ]

                    Nothing ->
                        attrs

            else
                attrs
    in
    S.g
        (withPossibleOnMouseDown groupAttrs
            ++ withPossibleOnMouseMove groupAttrs
            ++ [ SA.transform
                    ("translate("
                        ++ String.fromFloat node.pos.x
                        ++ " "
                        ++ String.fromFloat node.pos.y
                        ++ ")"
                    )
               ]
        )
        (nodeBackground node nodeAttrs
            ++ [ nodeForeground node nodeAttrs ]
            ++ [ S.map
                    (UpdateNodeContent node.id)
                    (nodeContentView model node.content nodeAttrs)
               ]
        )


nodeForeground : Node -> List (S.Attribute Msg) -> S.Svg Msg
nodeForeground node attributes =
    let
        withPossibleStrokeWidth attrs =
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
        (withPossibleStrokeWidth ([ SA.cursor cursor ] ++ attributes))


nodeContentView : Model -> NodeContent.Model -> List (S.Attribute Msg) -> S.Svg NodeContent.Msg
nodeContentView model nodeContent attrs =
    let
        retaggedAttrs =
            -- This is particularly gross -- any way to fix this?
            List.map
                (\attr ->
                    V.mapAttribute
                        (\msg1 ->
                            case model.state of
                                UpdatingNodeContent _ ->
                                    case msg1 of
                                        UpdateNodeContent _ subMsg ->
                                            subMsg

                                        _ ->
                                            NodeContent.doNothing

                                _ ->
                                    NodeContent.doNothing
                        )
                        attr
                )
                attrs
    in
    NodeContent.view nodeContent retaggedAttrs


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


describeModelState : Model -> String
describeModelState model =
    case model.state of
        UpdatingNodeContent nodeId ->
            "Updating content for node " ++ String.fromInt nodeId

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
